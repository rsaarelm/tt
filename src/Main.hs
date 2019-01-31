module Main where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Time
import           GHC.Exts
import           Numeric.Interval.NonEmpty
import           Options.Applicative
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.Exit
import           System.FilePath  (joinPath)
import           System.IO
import           Text.Printf
import           Tt.Entry
import           Tt.Goal
import qualified Tt.Msg                    as Msg
import           Tt.Parser
import           Tt.Util
import           Tt.Work
import           Options

main :: IO ()
main = do
  options <- execParser options
  -- This part is hacky. The prefix option has default value "~/", but we
  -- can't expand literal ~-paths, and when the user enters a ~-path on command
  -- line, the shell is expected to expand it to an absolute path. So the
  -- default value is special cased here to read the home directory.
  --
  -- The cleaner approach would be to just make the prefix option a Maybe
  -- String and have the branch for Nothing, but the string value is a bit
  -- more informative if it shows up in generated CLI options documentation.
  prefix <- case prefix options of
               "~/" -> getHomeDirectory
               x -> return x
  db <- sortOn entrySortKey . join <$> traverse (slurp prefix) ["done.txt", "todo.txt"]
  now <- getZonedTime
  runReaderT (runCmd $ cmd options) (Ctx now db (joinPath [prefix, "todo.txt"]))
 where
  -- Read file into entries
  slurp prefix file = do
   let path = joinPath [prefix, file]
   fileExists <- doesFileExist path
   if fileExists then mapMaybe parseEntry . lines <$> readFile path
                 else return []

type Db = [RawEntry]

data Ctx = Ctx {
    now :: ZonedTime
  , db :: !Db
  , todoPath :: FilePath
} deriving Show

type ContextIO a = ReaderT Ctx IO a

runCmd :: Cmd -> ContextIO ()
runCmd (In at project comment) = clockIn at project comment
runCmd (Out project comment) = clockOut project comment
runCmd (Break for comment) = projectBreak for comment
runCmd (Todo msg) = todo msg
runCmd (Done timestamp msg) = done timestamp msg
runCmd Timeclock = timeclock
runCmd Current = current
runCmd Goals = goals

adjustTime :: Maybe String -> ContextIO ZonedTime
adjustTime expr =
  case expr of
    Nothing -> asks now
    Just e -> handleTimeExpr e

handleTimeExpr :: String -> ContextIO ZonedTime
handleTimeExpr expr = do
  t <- asks now
  start <- currentProjectStart <$> loadUnsealedWork
  -- If the current work session starts later than current time, when figuring
  -- the end time for after we must use this as the start time.
  let lt = zonedTimeToLocalTime t
  let afterStartTime = fromMaybe lt start `max` lt

  case parseTimeExpr expr of
    Just (AbsoluteTime timeOfDay) ->
      return $ t {
        zonedTimeToLocalTime = lt {
          localTimeOfDay = timeOfDay
        }
      }
    Just (RelativeTime diff) -> return $ t {
        zonedTimeToLocalTime = diff `addLocalTime` lt
     }
    Just (AfterTotalTime diff) -> do
      workSoFar <- currentProjectToday
      return $ t {
        zonedTimeToLocalTime = (diff - workSoFar) `addLocalTime` afterStartTime
      }
    Just SinceSystemStartup -> do
      uptime <- liftIO systemUptime
      return $ t {
        zonedTimeToLocalTime = negate uptime `addLocalTime` lt
      }
    Nothing -> liftIO $ die (printf "Couldn't parse time expression '%s'" expr)

systemUptime :: IO NominalDiffTime
systemUptime = do
  -- XXX: Linux only
  uptime <- readFile "/proc/uptime"
  let uptimeSeconds = (read $ head $ words uptime) :: Float
  return $ secondsToNominalDiffTime $ fromIntegral $ truncate uptimeSeconds

clockIn :: Maybe String -> String -> Maybe String -> ContextIO ()
clockIn timeExpr project comment = do
  checkBreak
  checkPlanned
  t <- adjustTime timeExpr
  current <- getCurrentProject
  when (isJust current) (clockOut timeExpr Nothing)
  let msg = Msg.clockIn t project comment
  append msg
  liftIO $ printf "Clock in: %s\n" msg

clockOut :: Maybe String -> Maybe String -> ContextIO ()
clockOut timeExpr comment = do
  checkPlanned
  t <- adjustTime timeExpr
  current <- getCurrentProject
  startTime <- currentProjectStart <$> loadUnsealedWork
  -- TODO: See that checkout is later than start...
  case (current, startTime) of
    (Just project, Just start) | start > (zonedTimeToLocalTime t) ->
      liftIO $ die "Error: Trying to clock out before current session starts"
    (Just project, _) -> do
      let msg = Msg.clockOut t comment
      append msg
      liftIO $ printf "Clock out: %s\n" msg
    (Nothing, _) -> liftIO $ die "Error: Not clocked in a project."

projectBreak :: String -> Maybe String -> ContextIO ()
projectBreak timeExpr comment = do
  onBreak <- onBreak
  when (isJust onBreak) $ liftIO $ die "Already on break."
  current <- getCurrentProject
  t1      <- asks now
  t2      <- adjustTime (Just timeExpr)
  let (startTime, endTime) =
        if (zonedTimeToLocalTime t1 < zonedTimeToLocalTime t2)
          then (t1, t2)
          else (t2, t1)
  case current of
    Just project -> do
      append $ Msg.clockOut startTime comment
      append $ Msg.clockIn endTime project Nothing
      liftIO $ printf "On break from %s until %s.\n"
        project
        (formatTime defaultTimeLocale "%H:%M:%S" endTime)
    Nothing -> liftIO $ die "Not clocked in, nothing to take break from."

checkBreak :: ContextIO ()
checkBreak = do
  onBreak <- onBreak
  when (isJust onBreak) $ liftIO $ die "On scheduled break, clocking in not allowed."

checkPlanned :: ContextIO ()
checkPlanned = do
  onBreak <- onBreak
  inPlanned <- inPlanned
  when (isJust inPlanned && isNothing onBreak) $
    liftIO $ die "In pre-planned session, clocking in or out not allowed."

getCurrentProject :: ContextIO (Maybe String)
getCurrentProject = currentProject <$> loadWork

-- | Return if the log has a scheduled break that will end in the future.
--
-- Clocking in or out during break should not be allowed. If break is ongoing,
-- the return value will the time when the break is over.
onBreak :: ContextIO (Maybe LocalTime)
onBreak = do
  -- XXX: This will only detect a break before an open ClockIn item, not
  -- before a closed task set in the future.
  start <- currentProjectStart <$> loadUnsealedWork
  t <- asks (zonedTimeToLocalTime . now)
  return $ case start of
    Nothing -> Nothing
    Just s -> if t < s then Just s else Nothing

-- | Return if we're in a planned work session
inPlanned :: ContextIO (Maybe (Project, Session))
inPlanned = do
  work <- loadWork
  t <- asks (zonedTimeToLocalTime . now)
  return $ plannedProject work t

todo :: String -> ContextIO ()
todo msg = do
  t <- asks now
  let entry = Msg.todo t msg
  append entry
  liftIO $ printf "Todo task added: %s\n" entry

done :: Bool -> String -> ContextIO ()
done timestamp msg = do
  t <- asks now
  let entry = processMsg t msg
  append entry
  liftIO $ printf "Done task added: %s\n" entry
 where
  -- If the message starts with "^", backdate it to yesterday
  processMsg t c@('^':cs) = puntBack t c
  processMsg t c          = (if timestamp then Msg.doneWithTime else Msg.done)
                            t c
  puntBack t ('^':cs) = puntBack (yesterday t) cs
  puntBack t s        = Msg.done t s

timeclock :: ContextIO ()
timeclock = do
  work <- loadUnsealedWork
  mapM_ (liftIO . putStrLn) (timeClocks work)


current :: ContextIO ()
current = do
  work  <- loadWork
  today <- today
  t     <- asks now
  break <- onBreak
  case (break, currentProject work) of
    (Just until, Just project) -> do
      todaysTime <- currentProjectToday
      liftIO $ printf
        "%s %s, on break until %s\n" project (showHours todaysTime) (ftime until)
    (_, Just project) -> do
      todaysTime <- currentProjectToday
      liftIO $ printf "%s %s\n" project (showHours todaysTime)
    (_, Nothing) -> case plannedProject work (zonedTimeToLocalTime t) of
      Just (p, s) -> liftIO $ printf "%s until %s\n" p endTime
       where
        endTime = ftime $ sup (asTimeInterval s)
      Nothing -> return ()
   where
    ftime = formatTime defaultTimeLocale "%H:%M"

currentProjectToday :: ContextIO NominalDiffTime
currentProjectToday = do
  work <- loadWork
  today <- today
  return $ duration $ onCurrentProject work `during` today


goals :: ContextIO ()
goals = do
  goals <- loadGoals
  t     <- asks now
  unless (null goals) $ showGoals goals t
 where
  showGoals goals t = do
    liftIO $ printf
      "goal               current (target)   deadline                 failures\n"
    liftIO $ printf
      "------------------|------------------|------------------------|--------\n"
    mapM_ (printGoal t) goals

printGoal :: ZonedTime -> (Project, Goal) -> ContextIO ()
printGoal now (p, g) = liftIO $ printf
  "%-18s %-18s %-24s %s\n"
  p
  ( printf "%s %s %s"
           (showUnit (goalValue g) (goalUnit g))
           (if goalSlope g > 0 then "↑" else "↓")
           (showUnit (fromIntegral $ ceiling (goalTarget g')) (goalUnit g')) :: String
  )
  (Msg.deadline now (failureTime g))
  (if failureCount g > 0 then show (failureCount g) else "")
 where
  -- Show target point at next midnight where today's goal failure will be
  -- checked.
  g' = updateGoalClock g nextMidnight
  nextMidnight =
    LocalTime (1 `addDays` localDay (zonedTimeToLocalTime now)) midnight

append :: String -> ContextIO ()
append entry = do
  path <- asks todoPath
  liftIO $ appendFile path (entry ++ "\n")

loadWork :: ContextIO WorkState
loadWork = do
  work <- loadUnsealedWork
  now  <- asks now
  return $ seal now work

-- | Unsealed work will not have a session for the currently open project, but
-- it's what you want if you're printing timeclocks.
loadUnsealedWork :: ContextIO WorkState
loadUnsealedWork = toWorkState <$> asks db


loadGoals :: ContextIO [(Project, Goal)]
loadGoals = do
  work <- loadWork
  t  <- asks now
  return $ sortWith (\(_, g) -> failureTime g) $ map
    (\(p, g) -> (p, updateGoalClock g (zonedTimeToLocalTime t)))
    (activeGoals (entries work))

today :: ContextIO (Interval LocalTime)
today = do
  t <- asks (zonedTimeToLocalTime . now)
  return $ t { localTimeOfDay = midnight } ... t
