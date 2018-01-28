module Tt.Util (
    showRat
) where

import Data.Ratio
import Numeric

-- | Pretty-print a rational as decimal
showRat :: Rational -> String
showRat n =
    if denominator n == 1 then show (truncate n :: Integer)
    else showFFloat Nothing (fromRat n :: Double) ""
