module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Time
import           GHC.Exts
import           Numeric.Interval.NonEmpty
import           Options.Applicative
import           Text.Printf
import           Tt.Db
import           Tt.Entry
import           Tt.Goal
import qualified Tt.Msg                    as Msg
import           Tt.Util
import           Tt.Work

main :: IO ()
main =
  join
    $  execParser
    $  info (opts <**> helper)
    $  fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"

opts :: Parser (IO ())
opts =
  subparser
    $  command
         "in"
         ( info
             ( clockIn <$> argument str (metavar "project") <*> many
               (argument str (metavar "description"))
             )
         $ progDesc "Clock in to a project"
         )
    <> command
         "out"
         ( info (clockOut <$> many (argument str (metavar "description")))
         $ progDesc "Clock out of the clocked in project"
         )
    <> command
         "todo"
         ( info (todo <$> some (argument str (metavar "description")))
         $ progDesc "Add a todo item from the command line"
         )
    <> command
         "done"
         ( info (done <$> some (argument str (metavar "description")))
         $ progDesc "Add a done item from the command line"
         )
    <> command
         "timeclock"
         ( info (pure timeclock)
         $ progDesc "Output hours in timeclock format for hledger"
         )
    <> command
         "current"
         ( info (pure current)
         $ progDesc "Show current project and hours worked on it today"
         )
    <> command
         "balance"
         ( info (balance <$> optional (argument str (metavar "project")))
         $ progDesc "Show monthly flexitime balance on project"
         )
    <> command
         "goals"
         ( info (pure goals)
         $ progDesc "Show progress on currently active goals"
         )


clockIn :: String -> [String] -> IO ()
clockIn project text = do
  current <- getCurrentProject
  when (isJust current) (clockOut [])
  now <- getZonedTime
  append $ Msg.clockIn now project (unwords text)
  printf "Clocked into %s.\n" project

clockOut :: [String] -> IO ()
clockOut text = do
  current <- getCurrentProject
  case current of
    Just project -> do
      now <- getZonedTime
      append $ Msg.clockOut now (unwords text)
      printf "Clocked out of %s.\n" project
    Nothing -> printf "Error: Not clocked in a project.\n"

getCurrentProject :: IO (Maybe String)
getCurrentProject = do
  work <- loadWork
  return $ currentProject work


todo :: [String] -> IO ()
todo text = do
  now <- getZonedTime
  let msg = Msg.todo now (unwords text)
  append msg
  printf "Todo task added: %s\n" msg

done :: [String] -> IO ()
done text = do
  now <- getZonedTime
  let msg          = unwords text
  -- If the message starts with "^", backdate it to yesterday
  let (now', msg') = puntBack now msg
  let msg          = Msg.done now' msg'
  append msg
  printf "Done task added: %s\n" msg
 where
  puntBack t ('^':cs) = puntBack (yesterday t) cs
  puntBack t s        = (t, s)

timeclock :: IO ()
timeclock = do
  work <- loadUnsealedWork
  mapM_ putStrLn (timeClocks work)


current :: IO ()
current = do
  work  <- loadWork
  today <- today
  now   <- getZonedTime
  case currentProject work of
    Just project -> do
      let todaysTime = duration $ onCurrentProject work `during` today
      printf "%s %s\n" project (showHours todaysTime)
    Nothing -> case plannedProject work (zonedTimeToLocalTime now) of
      Just (p, s) -> printf "%s until %s\n" p endTime
       where
        endTime = formatTime defaultTimeLocale "%H:%M" $ sup (asTimeInterval s)
      Nothing -> return ()

balance :: Maybe String -> IO ()
balance proj = do
  thisMonth <- thisMonth
  today     <- today
  work      <- loadWork
  -- Some chances to drop out with a Nothing value, so drop into a Maybe monad
  ( fromMaybe (\_ -> return ()) $ do
      p        <- proj <|> currentProject work
      interval <- thisMonth `before` today
      let sessions     = work `onProject` p `during` interval

      let amountWorked = duration sessions
      let numDays      = daysCovered (map asTimeInterval sessions)
      let nominalHour  = nominalDay / 24
      let targetAmount = nominalHour * 7.5 * fromIntegral numDays
      let balance      = amountWorked - targetAmount
      return
        ( \_ -> do
          printf "%s hours over %d days this month in project %s\n"
                 (showHours amountWorked)
                 numDays
                 p
          printf "Flexitime balance %s\n" (showHours balance)
        )
    )
    ()


goals :: IO ()
goals = do
  goals <- loadGoals
  now   <- getZonedTime
  printf
    "goal               current (target)   deadline                 failures\n"
  printf
    "------------------|------------------|------------------------|--------\n"
  mapM_ (printGoal now) goals

printGoal :: ZonedTime -> (Project, Goal) -> IO ()
printGoal now (p, g) = printf
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


loadDb :: IO Db
loadDb = do
  conf <- dbConf
  readDb conf

append :: String -> IO ()
append entry = do
  conf <- dbConf
  dbAppend conf entry

loadWork :: IO WorkState
loadWork = do
  work <- loadUnsealedWork
  now  <- getZonedTime
  return $ seal now work

-- | Unsealed work will not have a session for the currently open project, but
-- it's what you want if you're printing timeclocks.
loadUnsealedWork :: IO WorkState
loadUnsealedWork = do
  db <- loadDb
  return (toWorkState db)

loadGoals :: IO [(Project, Goal)]
loadGoals = do
  work <- loadWork
  now  <- getZonedTime
  return $ sortWith (\(_, g) -> failureTime g) $ map
    (\(p, g) -> (p, updateGoalClock g (zonedTimeToLocalTime now)))
    (activeGoals (entries work))
