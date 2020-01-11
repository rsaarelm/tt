module Tt.Util (
  showRat,
  showHours,
  dayOf,
  monthOf,
  yesterday,
  before,
  daysCovered,
  addLocalTime,
  diffLocalTime,
  secondsToNominalDiffTime,
  nominalDiffTimeToSeconds,
  intervalDuration,
  AsTimeInterval(asTimeInterval),
  strip
) where

import           Data.Fixed
import           Data.Char
import           Data.List
import           Data.Ratio
import           Data.Time
import           Numeric
import           Numeric.Interval.NonEmpty
import           Text.Printf
import           Unsafe.Coerce

-- | Pretty-print a rational as decimal
showRat :: Rational -> String
showRat n = if denominator n == 1
  then show (truncate n :: Integer)
  else printf "%.1f" (fromRat n :: Double)

-- | Show a nicely formatted hour readout
showHours :: NominalDiffTime -> String
showHours d = printf "%.1f h" $ realToFrac d / (3600 :: Double)
-- | Convert time to the exact same time the previous day.

-- XXX: The calendar interval formulas are gross, can they be made cleaner?

-- | Time span for day of the given time stamp.
dayOf :: ZonedTime -> Interval LocalTime
dayOf now = start ... end
 where
  start = (zonedTimeToLocalTime now) { localTimeOfDay = midnight }
  end   = start { localDay = addDays 1 (localDay start) }

-- | Time span for the month of the given time stamp.
monthOf :: ZonedTime -> Interval LocalTime
monthOf now = start ... end
 where
  start = startOfMonth (zonedTimeToLocalTime now)
  end   = endOfMonth (zonedTimeToLocalTime now)
  startOfMonth t = LocalTime first midnight
   where
    first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1
  endOfMonth t = LocalTime (addGregorianMonthsClip 1 first) midnight
   where
    first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1

yesterday :: ZonedTime -> ZonedTime
yesterday t = t
  { zonedTimeToLocalTime = (zonedTimeToLocalTime t)
    { localDay = addDays (-1) (localDay (zonedTimeToLocalTime t))
    }
  }

-- | Return span before from calendar start to start of the given span.
--
-- Operates on interval instead of a single timepoint for API ergonomics.
before :: Interval LocalTime -> Interval LocalTime -> Maybe (Interval LocalTime)
a `before` b = a `intersection` (LocalTime (ModifiedJulianDay 0) midnight ... inf b)

-- | Count the number of separate days a session list covers
daysCovered :: [Interval LocalTime] -> Int
daysCovered = length . group . sort . concatMap sessionDays
 where
  sessionDays session = [day (inf session) .. day (sup session)]
  day t = toModifiedJulianDay (localDay t)


-- XXX: Copied from time 1.9, remove when stack can install 1.9
addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

-- XXX: Copied from time 1.9, remove when stack can install 1.9
diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

-- XXX: Also from time 1.9, doing things the hacky way since the proper way is
-- private in 1.8.
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = unsafeCoerce

-- XXX: More time 1.9 copy-paste
nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = unsafeCoerce

intervalDuration :: Interval LocalTime -> NominalDiffTime
intervalDuration i = sup i `diffLocalTime` inf i

class AsTimeInterval a where
  asTimeInterval :: a -> Interval LocalTime

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip []                   = []
lstrip (c : cs) | isSpace c = lstrip cs
lstrip s                    = s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
