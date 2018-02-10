module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Time
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
  case currentProject work of
    Just project -> do
      let todaysTime = duration $ onCurrentProject work `during` today
      printf "%s %s\n" project (showHours todaysTime)
    Nothing -> return ()


balance :: Maybe String -> IO ()
balance proj = do
  thisMonth <- thisMonth
  today     <- today
  work      <- loadWork
  -- Some chances to drop out with a Nothing value, so drop into a Maybe monad
  (fromMaybe (\_ -> return ()) $ do
        p        <- proj <|> currentProject work
        interval <- thisMonth `intersection` before today
        let sessions = work `onProject` p `during` interval

        let amountWorked = duration sessions
        let numDays = daysCovered (map asTimeInterval sessions)
        let nominalHour = nominalDay / 24
        let targetAmount = nominalHour * 7.5 * fromIntegral numDays
        let balance = amountWorked - targetAmount
        return (\_ -> do
          printf "%s hours over %d days this month in project %s\n"
                (showHours amountWorked) numDays p
          printf "Flexitime balance %s\n" (showHours balance))) ()


goals :: IO ()
goals = do
  work <- loadWork
  goals <- loadGoals
  now <- getZonedTime
  printf "Goal              done               ahead by\n"
  printf "-----------------|------------------|----------\n"
  mapM_ (printGoal work now) goals

printGoal :: WorkState -> ZonedTime -> Goal -> IO ()
printGoal work now goal = do
  let daysNow = goal `daysSpanned` now
  let current = work `towards` goal
  let start = goalStart current
  let target = goalTarget goal
  let total = totalWork current
  let progressDays = (total - start) * goalDays goal / (target - start)
  let dayScore = (truncate $ progressDays - daysNow) :: Integer
  printf "%-17s %-16s  % 5d day%s\n"
    (goalName goal)
    ( printf "%s -> %s"
            (showUnit total (goalUnit goal))
            (showUnit target (goalUnit goal))
     :: String)
    dayScore
    (if dayScore /= 1 then "s" else "")


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

loadGoals :: IO [Goal]
loadGoals = do
  db <- loadDb
  now <- getZonedTime
  return $ activeGoals now db
