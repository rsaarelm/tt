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
runCmd (Todo msg) = todo msg
runCmd (Done timestamp msg) = done timestamp msg
runCmd cmd =
  liftIO $ print "Hello world!"

adjustTime :: Maybe String -> ContextIO ZonedTime
adjustTime expr = do
  t <- now <$> ask
  case parseTimeAdjust t (fromMaybe "" expr) of
    Left err -> liftIO $ die err
    Right time -> return time

parseTimeAdjust :: ZonedTime -> String -> Either String ZonedTime
parseTimeAdjust now expr =
  -- TODO: Modify time now according to expr
  -- TODO: If expr does not parse as valid time adjustment, return error
  Right now

clockIn :: Maybe String -> String -> Maybe String -> ContextIO ()
clockIn timeExpr project comment = do
  checkBreak
  t <- adjustTime timeExpr
  current <- getCurrentProject
  when (isJust current) (clockOut timeExpr Nothing)
  append $ Msg.clockIn t project comment
  liftIO $ printf "Clocked into %s.\n" project

clockOut :: Maybe String -> Maybe String -> ContextIO ()
clockOut timeExpr comment = do
  checkBreak
  t <- adjustTime timeExpr
  current <- getCurrentProject
  case current of
    Just project -> do
      append $ Msg.clockOut t comment
      liftIO $ printf "Clocked out of %s.\n" project
    Nothing -> liftIO $ die "Error: Not clocked in a project."

checkBreak :: ContextIO ()
checkBreak = do
  onBreak <- onBreak
  when (isJust onBreak) $ liftIO $ die "On scheduled break, clocking in or out not allowed."

getCurrentProject :: ContextIO (Maybe String)
getCurrentProject = currentProject <$> loadWork

-- | Return if the log has a scheduled break that will end in the future.
--
-- Clocking in or out during break should not be allowed. If break is ongoing,
-- the return value will the time when the break is over.
onBreak :: ContextIO (Maybe LocalTime)
onBreak = do
  start <- currentProjectStart <$> loadUnsealedWork
  t <- zonedTimeToLocalTime . now <$> ask
  return $ case start of
    Nothing -> Nothing
    Just s -> if t < s then Just s else Nothing

todo :: String -> ContextIO ()
todo msg = do
  t <- now <$> ask
  let entry = Msg.todo t msg
  append entry
  liftIO $ printf "Todo task added: %s\n" entry

done :: Bool -> String -> ContextIO ()
done timestamp msg = do
  t <- now <$> ask
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

--timeclock :: IO ()
--timeclock = do
--  work <- loadUnsealedWork
--  mapM_ putStrLn (timeClocks work)
--
--
--current :: IO ()
--current = do
--  work  <- loadWork
--  today <- today
--  now   <- getZonedTime
--  case currentProject work of
--    Just project -> do
--      let todaysTime = duration $ onCurrentProject work `during` today
--      printf "%s %s\n" project (showHours todaysTime)
--    Nothing -> case plannedProject work (zonedTimeToLocalTime now) of
--      Just (p, s) -> printf "%s until %s\n" p endTime
--       where
--        endTime = formatTime defaultTimeLocale "%H:%M" $ sup (asTimeInterval s)
--      Nothing -> return ()
--
--
--goals :: IO ()
--goals = do
--  goals <- loadGoals
--  now   <- getZonedTime
--  unless (null goals) $ showGoals goals now
-- where
--  showGoals goals now = do
--    printf
--      "goal               current (target)   deadline                 failures\n"
--    printf
--      "------------------|------------------|------------------------|--------\n"
--    mapM_ (printGoal now) goals
--
--printGoal :: ZonedTime -> (Project, Goal) -> IO ()
--printGoal now (p, g) = printf
--  "%-18s %-18s %-24s %s\n"
--  p
--  ( printf "%s %s %s"
--           (showUnit (goalValue g) (goalUnit g))
--           (if goalSlope g > 0 then "↑" else "↓")
--           (showUnit (fromIntegral $ ceiling (goalTarget g')) (goalUnit g')) :: String
--  )
--  (Msg.deadline now (failureTime g))
--  (if failureCount g > 0 then show (failureCount g) else "")
-- where
--  -- Show target point at next midnight where today's goal failure will be
--  -- checked.
--  g' = updateGoalClock g nextMidnight
--  nextMidnight =
--    LocalTime (1 `addDays` localDay (zonedTimeToLocalTime now)) midnight

append :: String -> ContextIO ()
append entry = do
  path <- todoPath <$> ask
  liftIO $ appendFile path (entry ++ "\n")

loadWork :: ContextIO WorkState
loadWork = do
  work <- loadUnsealedWork
  now  <- now <$> ask
  return $ seal now work

-- | Unsealed work will not have a session for the currently open project, but
-- it's what you want if you're printing timeclocks.
loadUnsealedWork :: ContextIO WorkState
loadUnsealedWork = toWorkState <$> (db <$> ask)


--loadGoals :: IO [(Project, Goal)]
--loadGoals = do
--  work <- loadWork
--  now  <- getZonedTime
--  return $ sortWith (\(_, g) -> failureTime g) $ map
--    (\(p, g) -> (p, updateGoalClock g (zonedTimeToLocalTime now)))
--    (activeGoals (entries work))

today :: ContextIO (Interval LocalTime)
today = dayOf <$> (now <$> ask)
