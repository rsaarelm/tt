module Main where

import Control.Monad (join)
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath (joinPath)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time (getCurrentTimeZone)
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
  <> command "todo" (info (todo <$> many (argument str (metavar "description"))) $
      progDesc "Add a todo item from the command line")
  <> command "timeclock" (info (pure timeclock) $
      progDesc "Output hours in timeclock format for hledger")

in_ :: String -> [String] -> IO ()
in_ project text = do
    line <- clockInPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (line ++ tokenize project ++ tokenize (unwords text)) ++ "\n"
    putStrLn "Clocked in"

out :: [String] -> IO ()
out text = do
    line <- clockOutPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (line ++ tokenize (unwords text)) ++ "\n"
    putStrLn "Clocked out"

todo :: [String] -> IO ()
todo text = do
    line <- todoPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ showTokens (line ++ tokenize (unwords text)) ++ "\n"
    putStrLn "Task added"

timeclock :: IO ()
timeclock = do
    db <- readDatabase
    tz <- getCurrentTimeZone
    mapM_ (putStrLn . asTimeclock) $ toClockData tz db


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

todoFilePath :: IO FilePath
todoFilePath = homeFilePath "todo.txt"

doneFilePath :: IO FilePath
doneFilePath = homeFilePath "done.txt"

homeFilePath :: FilePath -> IO FilePath
homeFilePath filename = do
    home <- getHomeDirectory
    return $ joinPath [home, filename]
