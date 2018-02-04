module Tt.Util (
    showRat,
    yesterday
) where

import           Data.Ratio
import           Data.Time
import           Numeric

-- | Pretty-print a rational as decimal
showRat :: Rational -> String
showRat n = if denominator n == 1
  then show (truncate n :: Integer)
  else showFFloat Nothing (fromRat n :: Double) ""

-- | Convert time to the exact same time the previous day.
yesterday :: ZonedTime -> ZonedTime
yesterday t = t
  { zonedTimeToLocalTime = (zonedTimeToLocalTime t)
    { localDay = addDays (-1) (localDay (zonedTimeToLocalTime t))
    }
  }
