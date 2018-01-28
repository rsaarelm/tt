module Tt.Clock (
  ClockDb,
  clocks,
  currentProject,
  asTimeclock,
  clockInEntry,
  clockOutEntry,
) where

import Data.List
import Data.Maybe
import Data.Time
import Tt.Token
import Tt.Db

data ClockEntry =
    In ZonedTime String String
  | Out ZonedTime String
    deriving (Show)

type ClockDb = [ClockEntry]

-- | Convert database to sorted clock data
clocks :: Db -> ClockDb
clocks db = sortBy clockOrd clockLines
  where
    clockLines = mapMaybe toClockEntry db
    clockOrd c1 c2 = sortKey c1 `compare` sortKey c2
    sortKey :: ClockEntry -> (UTCTime, Int)
    sortKey (In t _ _) = (zonedTimeToUTC t, 1)
    sortKey (Out t _) = (zonedTimeToUTC t, 0)

-- | Show currently clocked project from a ClockEntry sequence.
-- NB: Function assumes the entries are sorted.
currentProject :: ClockDb -> Maybe String
currentProject [] = Nothing
currentProject [In _ name _] = Just name
currentProject (_:xs) = currentProject xs

-- | Show a ClockEntry as a timeclock log line.
asTimeclock :: ClockEntry -> String
-- Use unwords to avoid trailing whitespace when 'text' is empty.
asTimeclock (In t project text) = unwords $ ["i", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S%z" t, project] ++ words text
asTimeclock (Out t text)        = unwords $ ["o", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S%z" t] ++ words text


-- | Try to convert an Entry into a ClockEntry
toClockEntry :: Entry -> Maybe ClockEntry
toClockEntry (Sym "x":Date d:Time t:Sym "s":Sym p:ts) = Just $ In (makeZonedTime d t) p (showTokens ts)
toClockEntry (Sym "x":Date d:Time t:Sym "e":ts) = Just $ Out (makeZonedTime d t) (showTokens ts)
toClockEntry _ = Nothing

makeZonedTime :: Day -> (TimeOfDay, TimeZone) -> ZonedTime
makeZonedTime d (t, tz) = ZonedTime (LocalTime d t) tz

clockInEntry :: ZonedTime -> String -> String -> Entry
clockInEntry t proj text = [Sym "x"] ++ dateTime t ++ [Sym "s", Sym proj] ++ tokenize text

clockOutEntry :: ZonedTime -> String -> Entry
clockOutEntry t text = [Sym "x"] ++ dateTime t ++ [Sym "e"] ++ tokenize text


dateTime :: ZonedTime -> [Token]
dateTime t = [Date (localDay local), Time (localTimeOfDay local, zonedTimeZone t)]
  where
    local = zonedTimeToLocalTime t
