module Tt.Session (
  SessionDb,
  Session(sessionProject),
  sessions,
  sessionLength,
  daySpan,
  monthSpan,
  clamp,
  daysCovered,
  showHours,
) where

import           Data.List
import           Data.Time
import           Debug.Trace
import           Text.Printf
import           Tt.Clock

data Session = Session { sessionProject :: String, sessionStart :: UTCTime, sessionEnd :: UTCTime }

type SessionDb = [Session]

-- | Generate sessions given clock entries and current time.
-- The current time is needed to complete the last open session, if any, to the current timepoint.
--
-- NB: Current time is assumed to be larger than the time value of any clock entry.
-- NB: Clock entries are assumed to be sorted.
sessions :: ZonedTime -> ClockDb -> SessionDb
sessions _ [] = []
sessions zt (In t1 project _:Out t2 _:ts) = Session project (zonedTimeToUTC t1) (zonedTimeToUTC t2) : sessions zt ts
-- Started clock that hasn't closed yet, this is fine, but we can't do the span without a current time.
sessions zt [In t1 project _] = [Session project (zonedTimeToUTC t1) (zonedTimeToUTC zt)]
-- TODO: Proper warnings from bad data
sessions zt (Out {}:ts) = trace "Unmatched clock out" sessions zt ts
sessions zt (In {}:In t2 p t:ts) = trace "Unmatched clock in" sessions zt (In t2 p t:ts)

sessionLength :: Session -> NominalDiffTime
sessionLength Session { sessionStart = start, sessionEnd = end } = end `diffUTCTime` start

-- | Time span for the day of the given zoned time stamp.
daySpan :: ZonedTime -> (UTCTime, UTCTime)
daySpan zt =
    (zonedTimeToUTC start, zonedTimeToUTC end)
    where start = zt { zonedTimeToLocalTime = startOfDay (zonedTimeToLocalTime zt) }
          end = zt { zonedTimeToLocalTime = endOfDay (zonedTimeToLocalTime zt) }
          startOfDay t = t { localTimeOfDay = midnight }
          endOfDay t = LocalTime { localTimeOfDay = midnight, localDay = addDays 1 (localDay t) }

-- | Time span for the month of the given zoned time stamp.
monthSpan :: ZonedTime -> (UTCTime, UTCTime)
monthSpan zt =
    (zonedTimeToUTC start, zonedTimeToUTC end)
    where start = zt { zonedTimeToLocalTime = startOfMonth (zonedTimeToLocalTime zt) }
          end = zt { zonedTimeToLocalTime = endOfMonth (zonedTimeToLocalTime zt) }
          startOfMonth t = LocalTime first midnight
              where first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1
          endOfMonth t = LocalTime (addGregorianMonthsClip 1 first) midnight
              where first = let (y, m, _) = toGregorian (localDay t) in fromGregorian y m 1

-- | Clamp a work session into a range.
-- NB: Range pair is assumed to be sorted.
clamp :: (UTCTime, UTCTime) -> Session -> Maybe Session
clamp (start, _) Session { sessionEnd = end } | start >= end = Nothing
clamp (_, end) Session { sessionStart = start } | start >= end = Nothing
clamp (start, end) s@Session { sessionStart = start', sessionEnd = end' } =
    Just $ s { sessionStart = start `max` start', sessionEnd = end `min` end' }

-- | Count the number of separate days a session list covers
daysCovered :: TimeZone -> [Session] -> Int
daysCovered tz = length . group . sort . concatMap (sessionDays tz)

-- | Modified Julian days spanned by a session
sessionDays :: TimeZone -> Session -> [Integer]
sessionDays tz session =
    [utcDay (sessionStart session)..utcDay (sessionEnd session)]
    where utcDay u = toModifiedJulianDay $ localDay (utcToLocalTime tz u)

-- | Show a nicely formatted hour readout
showHours :: NominalDiffTime -> String
showHours d = printf "%.1f h" $ realToFrac d / (3600 :: Double)
