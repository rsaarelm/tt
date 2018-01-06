module Tt where

import qualified Data.Time as Time
import Data.Char

-- | Parts of a todo.txt line item
data Token =
    Text String             -- ^ A whitespace separated string that isn't any of the more specific categories
  | Identifier String       -- ^ A [_A-Za-z][_A-Za-z0-9]* string, standard identifier limits
  | Date Time.Day           -- ^ A calendar date, YYYY-mm-dd
  | Time Time.TimeOfDay     -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Colon Token Token       -- ^ Two tokens split by colon (the whole doesn't parse into Time)
    deriving (Eq, Show)

type Parse = Either Token String

-- Force time parser to return a Maybe-wrapped value of whatever time type we
-- want using the given format string.
timeParse :: (Time.ParseTime t) => String -> String -> Maybe t
timeParse = Time.parseTimeM True Time.defaultTimeLocale

parseDate :: String -> Parse
parseDate s = case timeParse "%Y-%m-%d" s of
    Nothing -> Right s
    Just d -> Left (Date d)

parseTime :: String -> Parse
parseTime s = case timeParse "%H:%M" s of
    Nothing -> Right s
    Just t -> Left (Time t)

parseTimeSec :: String -> Parse
parseTimeSec s = case timeParse "%H:%M:%S" s of
    Nothing -> Right s
    Just t -> Left (Time t)

parseProject :: String -> Parse
parseProject ('+' : xs) = Left (Project xs)
parseProject s = Right s

parseId :: String -> Parse
parseId s | isId s = Left (Identifier s)
    where
    isId (x:xs) | x == '_' || isAlpha x = isIdTail xs
    isId _ = False
    isIdTail [] = True
    isIdTail (x:xs) | x == '_' || isAlphaNum x = isIdTail xs
    isIdTail _ = False
parseId s = Right s

-- TODO
--parseColon :: String -> Parse
--parseColon s = case breakOn ":" s of
--    ((h : hs), (':' : t : ts)) -> Left (Colon $ parseToken (h : hs) $ parseToken (t : ts))
--    _ -> Right s

parseColon :: String -> Parse
parseColon = Right

parse :: String -> Parse
parse s = do
    s <- return s
    s <- parseDate s
    s <- parseTime s
    s <- parseTimeSec s
    s <- parseProject s
    s <- parseId s
    parseColon s

parseToken :: String -> Token
parseToken s = case parse s of
    Left token -> token
    Right s' -> Text s'
