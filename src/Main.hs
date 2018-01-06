module Main where

import System.Environment
import qualified Text.Parsec as Parsec
import qualified Data.Time as Time

-- | Parts of a todo.txt line item
data Token =
    Word String             -- ^ A regular whitespace separated string
  | Date Time.Day           -- ^ A calendar date, YYYY-mm-dd
  | Time Time.TimeOfDay     -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Attr String String      -- ^ A named attribute, "foo:bar" becomes (Attr "foo" "bar")
    deriving (Show)

parseToken :: String -> Token
parseToken = Word -- TODO

main :: IO ()
main = do
  -- TODO: Helpful error message when CLI fails
  [f] <- getArgs
  doneData <- readFile f
  let line = head $ lines doneData
  putStrLn line
