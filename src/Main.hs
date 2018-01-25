module Main where

import Control.Monad
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath (joinPath)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time
import Text.Printf
import Tt

main :: IO ()
main = join $ execParser $ info (opts <**> helper) $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"

opts :: Parser (IO ())
opts = subparser $
     command "in" (info (in_ <$> argument str (metavar "project") <*> many (argument str (metavar "description"))) $
      progDesc "Clock in to a project")
  <> command "out" (info (out <$> many (argument str (metavar "description"))) $
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

in_ :: String -> [String] -> IO ()
in_ project text = do
    current <- getCurrentProject
    when (isJust current) (out [""])
    line <- clockInPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (line ++ tokenize project ++ tokenize (unwords text)) ++ "\n"
    putStrLn $ "Clocked into " ++ project ++ "."

out :: [String] -> IO ()
out text = do
    line <- clockOutPrefix
    todoPath <- todoFilePath
    current <- getCurrentProject
    case current of Just project -> do
                        appendFile todoPath $ showTokens (line ++ tokenize (unwords text)) ++ "\n"
                        putStrLn $ "Clocked out of " ++ project ++ "."
                    Nothing -> putStrLn "Error: Not clocked in a project"

todo :: [String] -> IO ()
todo text = do
    line <- todoPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (line ++ tokenize (unwords text)) ++ "\n"
    putStrLn "Task added"

done :: [String] -> IO ()
done text = do
    line <- todoPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (tokenize "x" ++ line ++ tokenize (unwords text)) ++ "\n"
    putStrLn "Done task added"

timeclock :: IO ()
timeclock = do
    db <- readDatabase
    mapM_ (putStrLn . asTimeclock) $ toClockData db

current :: IO ()
current = do
    clocks <- fmap toClockData readDatabase
    zt <- getZonedTime
    let current = currentProject clocks
    let currentWork = filter (\x -> Just (sessionProject x) == current) (sessions zt clocks)
    case current of Just project -> do
                        let todaysWork = mapMaybe (clamp (daySpan zt)) currentWork
                        let todaysTime = sum $ map sessionLength todaysWork
                        printf "%s %s\n" project (showHours todaysTime)
                    Nothing -> return ()

balance :: String -> IO ()
balance project = do
    clocks <- fmap toClockData readDatabase
    zt <- getZonedTime
    let work = mapMaybe (clamp (thisMonthUntilYesterday zt)) (filter (\x -> sessionProject x == project) (sessions zt clocks))
    let numDays = daysCovered (zonedTimeZone zt) work
    let amountWorked = sum $ map sessionLength work
    let nominalHour = nominalDay / 24
    -- TODO: Make the expected daily hours configurable.
    let targetAmount = nominalHour * 7.5 * fromInteger (toInteger numDays)
    let balance = amountWorked - targetAmount
    printf "%s hours over %d days this month in project %s\n" (showHours amountWorked) numDays project
    printf "Flexitime balance %s\n" (showHours balance)


thisMonthUntilYesterday :: ZonedTime -> (UTCTime, UTCTime)
thisMonthUntilYesterday zt =
    (begin, end)
    where
        (begin, _) = monthSpan zt
        (end, _) = daySpan zt

parseFile :: FilePath -> IO [[Token]]
parseFile path = do
    exists <- doesFileExist path
    if exists then do
        contents <- readFile path
        return $ map tokenize $ lines contents
    else return []

readDatabase :: IO [[Token]]
readDatabase = do
    donePath <- doneFilePath
    done <- parseFile donePath
    todoPath <- todoFilePath
    todo <- parseFile todoPath
    return (done ++ todo)

getCurrentProject :: IO (Maybe String)
getCurrentProject = do
    clocks <- fmap toClockData readDatabase
    return $ currentProject clocks

todoFilePath :: IO FilePath
todoFilePath = homeFilePath "todo.txt"

doneFilePath :: IO FilePath
doneFilePath = homeFilePath "done.txt"

homeFilePath :: FilePath -> IO FilePath
homeFilePath filename = do
    home <- getHomeDirectory
    return $ joinPath [home, filename]
