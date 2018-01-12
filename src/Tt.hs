module Tt (
    ClockEntry,
    asTimeclock,
    toClockData,
    currentProject,
    todaySpan,
    thisMonthSpan,
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
castToClock :: [Token] -> Maybe ClockEntry
castToClock (Sym "x":Date d:Time t:Sym "s":Sym p:ts) = Just $ In (combineTimes d t) p (showTokens ts)
castToClock (Sym "x":Date d:Time t:Sym "s":Project p:ts) = Just $ In (combineTimes d t) p (showTokens ts)
castToClock (Sym "x":Date d:Time t:Sym "e":ts) = Just $ Out (combineTimes d t) (showTokens ts)
castToClock _ = Nothing

-- | Combine LocalTime date and ZonedTime time of day from the token string to a single ZonedTime
combineTimes :: Day -> ZonedTime -> ZonedTime
combineTimes day zonedTime = ZonedTime localTime tz
    where localTime = LocalTime day (localTimeOfDay (zonedTimeToLocalTime zonedTime))
          tz = zonedTimeZone zonedTime

sortKey :: ClockEntry -> (UTCTime, Int)
sortKey (In t _ _) = (zonedTimeToUTC t, 1)
sortKey (Out t _) = (zonedTimeToUTC t, 0)

-- | Show a ClockEntry as a timeclock log line.
asTimeclock :: ClockEntry -> String
-- Use unwords to avoid trailing whitespace when 'text' is empty.
asTimeclock (In t project text) = unwords $ ["i", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S%z" t, project] ++ words text
asTimeclock (Out t text)        = unwords $ ["o", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S%z" t] ++ words text

-- | Convert database to sorted clock data
toClockData :: [[Token]] -> [ClockEntry]
toClockData db = sortBy clockOrd clockLines
    where
        clockLines = mapMaybe castToClock db
        clockOrd c1 c2 = sortKey c1 `compare` sortKey c2

-- | Show currently clocked project from a ClockEntry sequence.
-- NB: Function assumes the entries are sorted.
currentProject :: [ClockEntry] -> Maybe String
currentProject [] = Nothing
currentProject [In _ name _] = Just name
currentProject (_:xs) = currentProject xs


data TimeSpan = TimeSpan { startTime :: UTCTime, endTime :: UTCTime }
    deriving (Show)

-- | Time span for this day in the local time zone.
todaySpan :: IO TimeSpan
todaySpan = do
    zt <- getZonedTime
    return TimeSpan { startTime = zonedTimeToUTC $ start zt, endTime = zonedTimeToUTC $ end zt}
    where start zt = zt { zonedTimeToLocalTime = startOfDay (zonedTimeToLocalTime zt) }
          end zt = zt { zonedTimeToLocalTime = endOfDay (zonedTimeToLocalTime zt) }

-- | Time span for this month in the local time zone.
thisMonthSpan :: IO TimeSpan
thisMonthSpan = do
    zt <- getZonedTime
    return TimeSpan { startTime = zonedTimeToUTC $ start zt, endTime = zonedTimeToUTC $ end zt}
    where start zt = zt { zonedTimeToLocalTime = startOfMonth (zonedTimeToLocalTime zt) }
          end zt = zt { zonedTimeToLocalTime = endOfMonth (zonedTimeToLocalTime zt) }

startOfDay :: LocalTime -> LocalTime
startOfDay t = t { localTimeOfDay = midnight }

endOfDay :: LocalTime -> LocalTime
endOfDay t = LocalTime { localTimeOfDay = midnight, localDay = addDays 1 (localDay t) }

startOfMonth :: LocalTime -> LocalTime
startOfMonth t = LocalTime first midnight
    where first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1

endOfMonth :: LocalTime -> LocalTime
endOfMonth t = LocalTime (addGregorianMonthsClip 1 first) midnight
    where first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1

-- | Parts of a todo.txt line item
data Token =
    Text String             -- ^ A whitespace separated string that isn't any of the more specific categories
  | Sym String              -- ^ A [_A-Za-z][_A-Za-z0-9]* symbolic identifier name
  | Date Day                -- ^ A calendar date, YYYY-mm-dd
  | Time ZonedTime          -- ^ A time of day, HH:MM:SS-HHMM
  | Project String          -- ^ A project tag, "+foo" becomes (Project "foo")
  | Colon Token Token       -- ^ Two tokens split by colon (the whole doesn't parse into Time)
    deriving (Show)


-- | Turn a string into a Token list
tokenize :: String -> [Token]
tokenize text = parseToken <$> words text

-- | Parse a single word (whitespace-delimited string) into a Tt Token.
-- This function doesn't check that the word has no whitespace, use 'tokenize'
-- instead to parse text with whitespace.
parseToken :: String -> Token
parseToken word = fromMaybe (Text word) $ parseDate word
                                      <|> parseTime word
                                      <|> parseProject word
                                      <|> parseSym word
                                      <|> parseColon word
  where
    parseDate s = Date <$> timeParse "%Y-%m-%d" s

    parseTime s = Time <$> timeParse "%H:%M:%S%z" s

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
dateTimeSeq :: ZonedTime -> [Token]
dateTimeSeq zt = [Date (localDay (zonedTimeToLocalTime zt)), Time zt]

-- | Get the current local time as Token sequence
currentDateTime :: IO [Token]
currentDateTime = do
    zt <- getZonedTime
    return (dateTimeSeq zt)

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
showToken (Time t) = formatTime defaultTimeLocale "%H:%M:%S%z" t
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


intersect :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
intersect (_, a) (b, _) | b >= a = Nothing
intersect (a, _) (_, b) | a >= b = Nothing
intersect (a1, a2) (b1, b2) = Just (a1 `max` b1, a2 `min` b2)
