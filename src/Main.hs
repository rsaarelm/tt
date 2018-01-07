module Main where

import Control.Monad (join)
import System.Directory (doesFileExist, getHomeDirectory)
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
    putStrLn $ showTokens (line ++ [parseToken project] ++ (map parseToken text))

out :: [String] -> IO ()
out text = do
    line <- clockOutPrefix
    putStrLn $ showTokens (line ++ (map parseToken text))

timeclock :: IO ()
timeclock = do
    putStrLn "TODO"


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
    home <- getHomeDirectory
    done <- parseFile (home ++ "/done.txt")
    todo <- parseFile (home ++ "/todo.txt")
    return (done ++ todo)
