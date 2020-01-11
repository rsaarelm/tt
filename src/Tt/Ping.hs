module Tt.Ping
  ( nextPing,
    pingTimes
  )
where

import           Data.Time
import           Data.Time.Clock.POSIX
import           Numeric.Interval.NonEmpty
import           Data.Int
import           Data.Bits
import           Tt.Util

nextPing :: NominalDiffTime -> UTCTime -> UTCTime
nextPing avgDuration now = posixSecondsToUTCTime
  (secondsToNominalDiffTime (fromIntegral t))
  where t = nextPing' (toInt64 avgDuration) (unixSeconds now)

pingTimes :: NominalDiffTime -> Interval UTCTime -> [UTCTime]
pingTimes avgDuration timeSpan =
  if next < sup timeSpan then
   (next : (pingTimes avgDuration (next ... sup timeSpan)))
  else
   []
 where
  next = nextPing avgDuration (inf timeSpan)


unixSeconds :: UTCTime -> Int64
unixSeconds = toInt64 . utcTimeToPOSIXSeconds

toInt64 :: NominalDiffTime -> Int64
toInt64 = fromIntegral . truncate . nominalDiffTimeToSeconds

nextPing' :: Int64 -> Int64 -> Int64
nextPing' intervalSeconds unixTime | isPing intervalSeconds (unixTime + 1) =
  unixTime + 1
nextPing' intervalSeconds unixTime = nextPing' intervalSeconds (unixTime + 1)

isPing :: Int64 -> Int64 -> Bool
isPing intervalSeconds unixTime = (noise unixTime) `mod` intervalSeconds == 0

-- Xorshift 64 noise formula
noise :: Int64 -> Int64
noise a = d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b 7)
  d = c `xor` (shiftL c 17)
