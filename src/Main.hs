module Main where

import Control.Monad (join)
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Tt

in_ :: String -> [String] -> IO ()
in_ project text = do
    putStrLn $ unwords (["s", project] ++ text)

out :: [String] -> IO ()
out text = do
    putStrLn $ unwords ("e" : text)

opts :: Parser (IO ())
opts = subparser $
     (command "in" $ info (in_ <$> (argument str (metavar "project")) <*> (many (argument str idm))) $
      progDesc "Clock in to a project")
  <> (command "out" $ info (out <$> many (argument str idm)) $
      progDesc "Clock out of the clocked in project")

main :: IO ()
main = join $ execParser $ info (opts <**> helper) $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"
