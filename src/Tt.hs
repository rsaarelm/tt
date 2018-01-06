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

type Parse = Either Token String

parseDate :: String -> Parse
parseDate s = case p of
        Nothing -> Right s
        Just d -> Left (Date d)
    where p = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" s :: Maybe Time.Day

parseTime :: String -> Parse
parseTime s = Right s -- TODO

parseWord :: String -> Parse
parseWord s = Left (Word s)

parse :: String -> Parse
parse s = do
    s' <- Right s
    s'' <- parseDate s'
    s''' <- parseTime s''
    return s'''

parseToken :: String -> Token
parseToken s = case parse s of
    Left token -> token
    Right s' -> Word s'
