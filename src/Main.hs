module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Time
import           Numeric.Interval.NonEmpty
import           Options.Applicative
import           Text.Printf
import           Tt.Clock
import           Tt.Db
import           Tt.Goal
import           Tt.Todo
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
  append $ clockInEntry now project (unwords text)
  printf "Clocked into %s.\n" project

clockOut :: [String] -> IO ()
clockOut text = do
  current <- getCurrentProject
  case current of
    Just project -> do
      now <- getZonedTime
      append $ clockOutEntry now (unwords text)
      printf "Clocked out of %s.\n" project
    Nothing -> printf "Error: Not clocked in a project.\n"

getCurrentProject :: IO (Maybe String)
getCurrentProject = do
  d   <- loadDb
  now <- getZonedTime
  return $ currentProject (parseWork now d)


todo :: [String] -> IO ()
todo text = do
  now <- getZonedTime
  let msg   = unwords text
  let entry = todoEntry now msg
  append entry
  printf "Todo task added: %s\n" (showTokens entry)

done :: [String] -> IO ()
done text = do
  now <- getZonedTime
  let msg          = unwords text
  -- If the message starts with "^", backdate it to yesterday
  let (now', msg') = puntBack now msg
  let entry        = doneEntry now' msg'
  append entry
  printf "Done task added: %s\n" (showTokens entry)
 where
  puntBack t ('^':cs) = puntBack (yesterday t) cs
  puntBack t s        = (t, s)

timeclock :: IO ()
timeclock = do
  clockDb <- clocks <$> loadDb
  mapM_ (putStrLn . asTimeclock) clockDb


current :: IO ()
current = do
  db  <- loadDb
  now <- getZonedTime
  let work = parseWork now db
  case currentProject work of
    Just project -> do
      let todaysTime = duration $ onCurrentProject work `during` today now
      printf "%s %s\n" project (showHours todaysTime)
    Nothing -> return ()


-- XXX: This could be less messy...
balance :: Maybe String -> IO ()
balance proj = do
  db  <- loadDb
  now <- getZonedTime
  let work = parseWork now db
  case proj <|> currentProject work of
    Just project -> printBalance now work project
    Nothing      -> putStrLn "No project specified or currently clocked in."
 where
  printBalance now work project =
    case thisMonth now `intersection` before (today now) of
      Just t  -> printBalance' t work project
      Nothing -> return ()
  printBalance' :: Interval LocalTime -> WorkState -> String -> IO ()
  printBalance' t work project = do
    let stuff        = work `onProject` project `during` t
    let amountWorked = duration stuff
    let numDays      = daysCovered (mapMaybe workInterval stuff)
    let nominalHour  = nominalDay / 24
    -- TODO: Make the expected daily hours configurable.
    let targetAmount = nominalHour * 7.5 * fromIntegral numDays
    let balance      = amountWorked - targetAmount
    printf "%s hours over %d days this month in project %s\n"
           (showHours amountWorked)
           numDays
           project
    printf "Flexitime balance %s\n" (showHours balance)



goals :: IO ()
goals = do
  entryDb <- loadDb
  now     <- getZonedTime
  let today = (localDay . zonedTimeToLocalTime) now
  let goals = activeGoals entryDb today
  printf "Goal              done           ahead by\n"
  printf "-----------------|--------------|----------\n"
  mapM_ (printGoal entryDb today) goals

printGoal :: Db -> Day -> Goal -> IO ()
printGoal db day goal = printf
  "%-17s %-12s  % 5d day%s\n"
  (goalName goal)
  ( printf "%s -> %s %s"
           (showRat currentProgress)
           (showRat $ goalTarget goal)
           (fromMaybe "" (goalUnit goal)) :: String
  )
  dayScore
  (if dayScore /= 1 then "s" else "")
 where
  (currentProgress, daysAhead) = progressStats db day goal
  dayScore                     = truncate daysAhead :: Int


loadDb :: IO Db
loadDb = do
  conf <- dbConf
  readDb conf

append :: Entry -> IO ()
append entry = do
  conf <- dbConf
  dbAppend conf entry
