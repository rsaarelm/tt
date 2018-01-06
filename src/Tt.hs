module Tt where

import qualified Data.Time as Time

-- | Parts of a todo.txt line item
data Token =
    Word String             -- ^ A regular whitespace separated string
  | Date Time.Day           -- ^ A calendar date, YYYY-mm-dd
  | Time Time.TimeOfDay     -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Attr String Token       -- ^ A named attribute, "foo:bar" becomes (Attr "foo" "bar")
    deriving (Eq, Show)

parseToken :: String -> Token
parseToken = Word -- TODO
