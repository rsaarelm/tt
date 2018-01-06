module Main where

import System.Environment
import Tt

main :: IO ()
main = do
  -- TODO: Helpful error message when CLI fails
  [f] <- getArgs
  doneData <- readFile f
  let line = head $ lines doneData
  print $ showTokens $ map parseToken $ words line
