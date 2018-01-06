module Tt where

import qualified Data.Time as Time
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

-- | Parts of a todo.txt line item
data Token =
    Text String             -- ^ A whitespace separated string that isn't any of the more specific categories
  | Identifier String       -- ^ A [_A-Za-z][_A-Za-z0-9]* string, standard identifier limits
  | Date Time.Day           -- ^ A calendar date, YYYY-mm-dd
  | Time Time.TimeOfDay     -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Colon Token Token       -- ^ Two tokens split by colon (the whole doesn't parse into Time)
    deriving (Eq, Show)

-- Force time parser to return a Maybe-wrapped value of whatever time type we
-- want using the given format string.
timeParse :: (Time.ParseTime t) => String -> String -> Maybe t
timeParse = Time.parseTimeM True Time.defaultTimeLocale

parseDate :: String -> Maybe Token
parseDate s = Date <$> timeParse "%Y-%m-%d" s

parseTime :: String -> Maybe Token
parseTime s = Time <$> timeParse "%H:%M" s

parseTimeSec :: String -> Maybe Token
parseTimeSec s = Time <$> timeParse "%H:%M:%S" s

parseProject :: String -> Maybe Token
parseProject ('+' : xs) = Just (Project xs)
parseProject _ = Nothing

parseId :: String -> Maybe Token
parseId (x:xs) | isFirst x && all isSecond xs = Just (Identifier (x:xs))
    where
    isFirst c = c == '_' || isAlpha c
    isSecond c = c == '_' || isAlphaNum c
parseId _ = Nothing

parseColon :: String -> Maybe Token
parseColon s = case elemIndex ':' s of
    Just idx -> case splitAt idx s of
        (x:xs, ':':y:ys) -> Just (Colon (parseToken (x:xs)) (parseToken (y:ys)))
        _ -> Nothing
    Nothing -> Nothing

parseToken :: String -> Token
parseToken s = fromMaybe (Text s) $ parseDate s
                                <|> parseTime s
                                <|> parseTimeSec s
                                <|> parseProject s
                                <|> parseId s
                                <|> parseColon s
