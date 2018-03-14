module Tt.Util (
  showRat,
  showHours,
  today,
  thisMonth,
  yesterday,
  before,
  daysCovered,
  addLocalTime,
  diffLocalTime,
  intervalDuration,
  AsTimeInterval(asTimeInterval)
) where

import           Data.List
import           Data.Ratio
import           Data.Time
import           Numeric
import           Numeric.Interval.NonEmpty
import           Text.Printf

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

today :: IO (Interval LocalTime)
today = dayOf <$> getZonedTime

-- | Time span for day of the given time stamp.
dayOf :: ZonedTime -> Interval LocalTime
dayOf now = start ... end
 where
  start = (zonedTimeToLocalTime now) { localTimeOfDay = midnight }
  end   = start { localDay = addDays 1 (localDay start) }

thisMonth :: IO (Interval LocalTime)
thisMonth = monthOf <$> getZonedTime

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

intervalDuration :: Interval LocalTime -> NominalDiffTime
intervalDuration i = sup i `diffLocalTime` inf i

class AsTimeInterval a where
  asTimeInterval :: a -> Interval LocalTime
