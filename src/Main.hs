module Main where

import System.Environment

main :: IO ()
main = do
  -- TODO: Helpful error message when CLI fails
  [f] <- getArgs
  doneData <- readFile f
  let line = head $ lines doneData
  putStrLn line
