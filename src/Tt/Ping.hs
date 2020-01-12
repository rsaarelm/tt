module Tt.Ping
  ( nextPing
  , lastPing
  , pingTimes
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
  where t = findPing (toInt64 avgDuration) 1 (unixSeconds now)

lastPing :: NominalDiffTime -> UTCTime -> UTCTime
lastPing avgDuration now = posixSecondsToUTCTime
  (secondsToNominalDiffTime (fromIntegral t))
  where t = findPing (toInt64 avgDuration) (-1) (unixSeconds now)

pingTimes :: NominalDiffTime -> Interval UTCTime -> [UTCTime]
pingTimes avgDuration timeSpan = if next < sup timeSpan
  then (next : (pingTimes avgDuration (next ... sup timeSpan)))
  else []
  where next = nextPing avgDuration (inf timeSpan)

unixSeconds :: UTCTime -> Int64
unixSeconds = toInt64 . utcTimeToPOSIXSeconds

toInt64 :: NominalDiffTime -> Int64
toInt64 = fromIntegral . truncate . nominalDiffTimeToSeconds

findPing :: Int64 -> Int64 -> Int64 -> Int64
findPing intervalSeconds delta unixTime
  | isPing intervalSeconds (unixTime + delta) = unixTime + delta
findPing intervalSeconds delta unixTime =
  findPing intervalSeconds delta (unixTime + delta)

isPing :: Int64 -> Int64 -> Bool
isPing intervalSeconds unixTime = (noise unixTime) `mod` intervalSeconds == 0

-- Xorshift 64 noise formula
noise :: Int64 -> Int64
noise a = d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b 7)
  d = c `xor` (shiftL c 17)
