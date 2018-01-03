module Main where

import System.Environment
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  -- TODO: Helpful error message when CLI fails
  [f] <- getArgs
  doneData <- readFile f
  let line = head $ lines doneData
  putStrLn line
