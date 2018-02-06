module Tt.Clock (
  clocks,
  clockInEntry,
  clockOutEntry,
) where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Debug.Trace
import           Numeric.Interval.NonEmpty
import           Tt.Db
import           Tt.Token

data ClockDirection = Out | In String deriving (Eq, Ord, Show)

-- | Convert database to sorted clock data and an optional currently open
--   project.
clocks :: Db -> ([(String, Interval LocalTime)], Maybe (String, LocalTime))
clocks db = openAndIntervals parseClocks
 where
  -- Parse entries and sort. Reverse it so that newest is first for the next
  -- step
  parseClocks = sortBy (flip compare) (mapMaybe parseEntry db)
  -- If the newest item opens a work session, grab that as the special open
  -- project value. Then reverse the remaining list back to chronological
  -- order and extract work session pairs.
  openAndIntervals ((begin, In project):revTs) =
    (clockWork (reverse revTs), Just (project, begin))
  openAndIntervals revTs = (clockWork (reverse revTs), Nothing)
  -- TODO: Proper error handling for mismatched clocks, trace is not how
  -- you're supposed to report user errors.
  clockWork ((begin, In project):(end, Out):ts) =
    (project, begin ... end) : clockWork ts
  clockWork ((_, In _):ts) = trace "Unmatched clock in" clockWork ts
  clockWork ((_, Out ):ts) = trace "Unmatched clock out" clockWork ts
  clockWork []             = []

parseEntry :: Entry -> Maybe (LocalTime, ClockDirection)
parseEntry (Sym "x":Date d:Time t _:Sym "s":Sym p:_) =
  Just (LocalTime d t, In p)
parseEntry (Sym "x":Date d:Time t _:Sym "e":_) = Just (LocalTime d t, Out)
parseEntry _ = Nothing

clockInEntry :: ZonedTime -> String -> String -> Entry
clockInEntry t proj text =
  [Sym "x"] ++ dateTime t ++ [Sym "s", Sym proj] ++ tokenize text

clockOutEntry :: ZonedTime -> String -> Entry
clockOutEntry t text = [Sym "x"] ++ dateTime t ++ [Sym "e"] ++ tokenize text

dateTime :: ZonedTime -> [Token]
dateTime t =
  [Date (localDay local), Time (localTimeOfDay local) (Just $ zonedTimeZone t)]
  where local = zonedTimeToLocalTime t
