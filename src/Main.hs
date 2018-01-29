module Main where

import Control.Monad
import Data.Maybe
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time
import Text.Printf
import Tt.Clock
import Tt.Db
import Tt.Session
import Tt.Todo

main :: IO ()
main = join $ execParser $ info (opts <**> helper) $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"

opts :: Parser (IO ())
opts = subparser $
     command "in" (info (clockIn <$> argument str (metavar "project") <*> many (argument str (metavar "description"))) $
      progDesc "Clock in to a project")
  <> command "out" (info (clockOut <$> many (argument str (metavar "description"))) $
      progDesc "Clock out of the clocked in project")
  <> command "todo" (info (todo <$> some (argument str (metavar "description"))) $
      progDesc "Add a todo item from the command line")
  <> command "done" (info (done <$> some (argument str (metavar "description"))) $
      progDesc "Add a done item from the command line")
  <> command "timeclock" (info (pure timeclock) $
      progDesc "Output hours in timeclock format for hledger")
  <> command "current" (info (pure current) $
      progDesc "Show current project and hours worked on it today")
  <> command "balance" (info (balance <$> argument str (metavar "project")) $
      progDesc "Show monthly flexitime balance on project")


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
    clockDb <- fmap clocks db
    return $ currentProject clockDb


todo :: [String] -> IO ()
todo text = do
    now <- getZonedTime
    let msg = unwords text
    let entry = todoEntry now msg
    append entry
    printf "Todo task added: %s\n" (showTokens entry)

done :: [String] -> IO ()
done text = do
    now <- getZonedTime
    let msg = unwords text
    let entry = doneEntry now msg
    append entry
    printf "Done task added: %s\n" (showTokens entry)


timeclock :: IO ()
timeclock = do
    clockDb <- clocks <$> db
    mapM_ (putStrLn . asTimeclock) clockDb


current :: IO ()
current = do
    clockDb <- clocks <$> db
    now <- getZonedTime
    let current = currentProject clockDb
    let currentWork = filter (\x -> Just (sessionProject x) == current) (sessions now clockDb)
    case current of
        Just project -> do
            let todaysWork = mapMaybe (clamp (daySpan now)) currentWork
            let todaysTime = sum $ map sessionLength todaysWork
            printf "%s %s\n" project (showHours todaysTime)
        Nothing -> return ()


balance :: String -> IO ()
balance project = do
    clockDb <- clocks <$> db
    now <- getZonedTime
    let work = mapMaybe (clamp (untilYesterday now)) (projectSessions now clockDb)
    let numDays = daysCovered (zonedTimeZone now) work
    let amountWorked = sum $ map sessionLength work
    let nominalHour = nominalDay / 24
    -- TODO: Make the expected daily hours configurable.
    let targetAmount = nominalHour * 7.5 * fromIntegral numDays
    let balance = amountWorked - targetAmount
    printf "%s hours over %d days this month in project %s\n" (showHours amountWorked) numDays project
    printf "Flexitime balance %s\n" (showHours balance)
  where
    projectSessions now clockDb =
        filter (\x -> sessionProject x == project) (sessions now clockDb)

untilYesterday :: ZonedTime -> (UTCTime, UTCTime)
untilYesterday t =
    (begin, end)
    where
        (begin, _) = monthSpan t
        (end, _) = daySpan t


db :: IO Db
db = do
    conf <- dbConf
    readDb conf

append :: Entry -> IO ()
append entry = do
    conf <- dbConf
    dbAppend conf entry
