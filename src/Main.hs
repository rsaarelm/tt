module Main where

import Control.Monad (join)
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath (joinPath)
import Options.Applicative
import Data.Semigroup ((<>))
import Tt

main :: IO ()
main = join $ execParser $ info (opts <**> helper) $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"

opts :: Parser (IO ())
opts = subparser $
     (command "in" $ info (in_ <$> (argument str (metavar "project")) <*> (many (argument str (metavar "description")))) $
      progDesc "Clock in to a project")
  <> (command "out" $ info (out <$> many (argument str (metavar "description"))) $
      progDesc "Clock out of the clocked in project")
  <> (command "timeclock" $ info (pure timeclock) $
      progDesc "Output hours in timeclock format for hledger")

in_ :: String -> [String] -> IO ()
in_ project text = do
    line <- clockInPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ (showTokens (line ++ [parseToken project] ++ (map parseToken text))) ++ "\n"
    putStrLn "Clocked in"

out :: [String] -> IO ()
out text = do
    line <- clockOutPrefix
    todoPath <- todoFilePath
    appendFile todoPath $ (showTokens (line ++ (map parseToken text))) ++ "\n"
    putStrLn "Clocked out"

timeclock :: IO ()
timeclock = do
    db <- readDatabase
    mapM_ putStrLn $ map asTimeclock $ mapMaybe castToClock db


parseFile :: FilePath -> IO [[Token]]
parseFile path = do
    exists <- doesFileExist path
    if exists then do
        contents <- readFile path
        return $ map tokenize $ lines contents
    else do
        return []

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
