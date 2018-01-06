module Tt where

import qualified Data.Time as Time
import Data.Char
import Data.Maybe
import Control.Applicative

-- | Parts of a todo.txt line item
data Token =
    Text String             -- ^ A whitespace separated string that isn't any of the more specific categories
  | Sym String              -- ^ A [_A-Za-z][_A-Za-z0-9]* symbolic identifier name
  | Date Time.Day           -- ^ A calendar date, YYYY-mm-dd
  | Time Time.TimeOfDay     -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Colon Token Token       -- ^ Two tokens split by colon (the whole doesn't parse into Time)
    deriving (Eq, Show)

-- | Pretty-print a Token
showToken :: Token -> String
showToken (Text t) = t
showToken (Sym t) = t
showToken (Date d) = show d
showToken (Time t) = show t
showToken (Project p) = "+" ++ p
showToken (Colon t u) = (show t) ++ ":" ++ (show u)

-- | Pretty-print a Token List
showTokens :: [Token] -> String
showTokens [] = ""
showTokens (x:[]) = showToken x
showTokens (x:xs) = showToken x ++ " " ++ showTokens xs

-- | Parse a single word (whitespace-delimited string) into a Tt Token.
parseToken :: String -> Token
parseToken word = fromMaybe (Text word) $ parseDate word
                                      <|> parseTime word
                                      <|> parseTimeSec word
                                      <|> parseProject word
                                      <|> parseSym word
                                      <|> parseColon word
  where
    parseDate s = Date <$> timeParse "%Y-%m-%d" s

    parseTime s = Time <$> timeParse "%H:%M" s

    parseTimeSec s = Time <$> timeParse "%H:%M:%S" s

    parseProject ('+' : xs) = Just (Project xs)
    parseProject _ = Nothing

    parseSym (x:xs) | isFirst x && all isSecond xs = Just (Sym (x:xs))
        where
        isFirst c = c == '_' || isAlpha c
        isSecond c = c == '_' || isAlphaNum c
    parseSym _ = Nothing

    parseColon s = case break (== ':') s of
        (x:xs, ':':y:ys) -> Just (Colon (parseToken (x:xs)) (parseToken (y:ys)))
        _ -> Nothing

    -- Force time parser to return a Maybe-wrapped value of whatever time type we
    -- want using the given format string.
    timeParse :: (Time.ParseTime t) => String -> String -> Maybe t
    timeParse = Time.parseTimeM True Time.defaultTimeLocale
