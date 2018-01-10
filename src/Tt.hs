module Tt (
    ClockEntry,
    asTimeclock,
    toClockData,
    currentProject,
    Token,
    tokenize,
    showTokens,
    todoPrefix,
    clockInPrefix,
    clockOutPrefix,
) where

import Data.Time hiding (parseTime)
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative

data ClockEntry =
    In ZonedTime String String
  | Out ZonedTime String
    deriving (Show)

-- | Try to a Token line into a ClockEntry.
castToClock :: TimeZone -> [Token] -> Maybe ClockEntry
castToClock tz (Sym "x":Date d:Time t:Sym "s":Sym p:ts) = Just $ In (ZonedTime (LocalTime d t) tz) p (showTokens ts)
castToClock tz (Sym "x":Date d:Time t:Sym "s":Project p:ts) = Just $ In (ZonedTime (LocalTime d t) tz) p (showTokens ts)
castToClock tz (Sym "x":Date d:Time t:Sym "e":ts) = Just $ Out (ZonedTime (LocalTime d t) tz) (showTokens ts)
castToClock _ _ = Nothing

sortKey :: ClockEntry -> (UTCTime, Int)
sortKey (In t _ _) = (zonedTimeToUTC t, 0)
sortKey (Out t _) = (zonedTimeToUTC t, 1)

-- | Show a ClockEntry as a timeclock log line.
asTimeclock :: ClockEntry -> String
-- Use unwords to avoid trailing whitespace when 'text' is empty.
asTimeclock (In t project text) = unwords $ ["i", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" (zonedTimeToLocalTime t), project] ++ words text
asTimeclock (Out t text)        = unwords $ ["o", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" (zonedTimeToLocalTime t)] ++ words text

-- | Convert database to sorted clock data
toClockData :: TimeZone -> [[Token]] -> [ClockEntry]
toClockData tz db = sortBy clockOrd clockLines
    where
        clockLines = mapMaybe (castToClock tz) db
        clockOrd c1 c2 = sortKey c1 `compare` sortKey c2

-- | Show currently clocked project from a ClockEntry sequence.
-- NB: Function assumes the entries are sorted.
currentProject :: [ClockEntry] -> Maybe String
currentProject [] = Nothing
currentProject [In _ name _] = Just name
currentProject (_:xs) = currentProject xs


-- | Parts of a todo.txt line item
data Token =
    Text String             -- ^ A whitespace separated string that isn't any of the more specific categories
  | Sym String              -- ^ A [_A-Za-z][_A-Za-z0-9]* symbolic identifier name
  | Date Day                -- ^ A calendar date, YYYY-mm-dd
  | Time TimeOfDay          -- ^ A time of day, HH:MM[:SS]
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Colon Token Token       -- ^ Two tokens split by colon (the whole doesn't parse into Time)
    deriving (Eq, Show)


-- | Turn a string into a Token list
tokenize :: String -> [Token]
tokenize text = parseToken <$> words text

-- | Parse a single word (whitespace-delimited string) into a Tt Token.
-- This function doesn't check that the word has no whitespace, use 'tokenize'
-- instead to parse text with whitespace.
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
    timeParse :: (ParseTime t) => String -> String -> Maybe t
    timeParse = parseTimeM True defaultTimeLocale


-- | Create standard Token sequence for date and time.
dateTimeSeq :: LocalTime -> [Token]
dateTimeSeq lt = [Date (localDay lt), Time (localTimeOfDay lt)]

-- | Get the current local time as Token sequence
currentDateTime :: IO [Token]
currentDateTime = do
    zt <- getZonedTime
    return (dateTimeSeq (zonedTimeToLocalTime zt))

-- | Line prefix for time log clock in action.
clockInPrefix :: IO [Token]
clockInPrefix = do
    dt <- currentDateTime
    return $ [Sym "x"] ++ dt ++ [Sym "s"]

-- | Line prefix for time log clock out action.
clockOutPrefix :: IO [Token]
clockOutPrefix = do
    dt <- currentDateTime
    return $ [Sym "x"] ++ dt ++ [Sym "e"]

todoPrefix :: IO [Token]
todoPrefix = do
    day <- today
    return [Date day]

-- | Pretty-print a Token
showToken :: Token -> String
showToken (Text t) = t
showToken (Sym t) = t
showToken (Date d) = show d
-- Don't show fractional seconds.
showToken (Time t) = formatTime defaultTimeLocale "%H:%M:%S" t
showToken (Project p) = "+" ++ p
showToken (Colon t u) = show t ++ ":" ++ show u

-- | Pretty-print a Token List
showTokens :: [Token] -> String
showTokens [] = ""
showTokens [x] = showToken x
showTokens (x:xs) = showToken x ++ " " ++ showTokens xs


today :: IO Day
today = do
    zt <- getZonedTime
    return $ localDay (zonedTimeToLocalTime zt)
