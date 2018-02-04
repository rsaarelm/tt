module Tt.Util (
  showRat,
  showHours,
  today,
  thisMonth,
  yesterday,
  before,
  daysCovered,
  UTCOrd(UTCOrd),
  fromUTCOrd,
  mapUTCOrd
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
  else showFFloat Nothing (fromRat n :: Double) ""

-- | Show a nicely formatted hour readout
showHours :: NominalDiffTime -> String
showHours d = printf "%.1f h" $ realToFrac d / (3600 :: Double)
-- | Convert time to the exact same time the previous day.

-- XXX: The calendar interval formulas are gross, can they be made cleaner?

-- | Time span for day of the given time stamp.
today :: ZonedTime -> Interval LocalTime
today now = start ... end
 where
  start = (zonedTimeToLocalTime now) { localTimeOfDay = midnight }
  end   = start { localDay = addDays 1 (localDay start) }

-- | Time span for the month of the given time stamp.
thisMonth :: ZonedTime -> Interval LocalTime
thisMonth now = start ... end
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
-- Operates on interval instead of a single UTCOrd for API ergonomics.
before :: Interval LocalTime -> Interval LocalTime
before s = LocalTime (ModifiedJulianDay 0) midnight ... inf s

-- | Count the number of separate days a session list covers
daysCovered :: [Interval UTCOrd] -> Int
daysCovered = length . group . sort . concatMap sessionDays
 where
  sessionDays session = [day (inf session) .. day (sup session)]
  day (UTCOrd t) = toModifiedJulianDay (localDay $ zonedTimeToLocalTime t)

newtype UTCOrd = UTCOrd ZonedTime
instance Ord UTCOrd where
    compare (UTCOrd x) (UTCOrd y) = compare (zonedTimeToUTC x) (zonedTimeToUTC y)
instance Eq UTCOrd where
    (UTCOrd x) == (UTCOrd y) = zonedTimeToUTC x == zonedTimeToUTC y

fromUTCOrd :: UTCOrd -> ZonedTime
fromUTCOrd (UTCOrd x) = x

mapUTCOrd :: (UTCTime -> UTCTime) -> UTCOrd -> UTCOrd
mapUTCOrd f a = UTCOrd $ utcToZonedTime zone $ f
  (zonedTimeToUTC (fromUTCOrd a))
  where zone = zonedTimeZone (fromUTCOrd a)
