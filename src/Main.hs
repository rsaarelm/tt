module Main where

import Control.Monad (join)
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Tt

in_ :: String -> IO ()
in_ project = do
    putStrLn $ "in " ++ project

out :: IO ()
out = do
    putStrLn "out!"

opts :: Parser (IO ())
opts = subparser $
     (command "in" $ info (in_ <$> argument str idm) $
      progDesc "Clock in to a project")
  <> (command "out" $ info (pure out) $
      progDesc "Clock out of the clocked in project")

main :: IO ()
main = join $ execParser $ info (opts <**> helper) $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"
