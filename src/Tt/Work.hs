module Tt.Work (
  WorkState(entries),
  toWorkState,
  seal,
  timeClocks,
  currentProject,
  currentProjectStart,
  plannedProject,
  intersectWork,
  duration,
  during,
  onProject,
  onCurrentProject
) where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Debug.Trace
import           Numeric.Interval.NonEmpty
import           Tt.Entry
import qualified Tt.Msg                    as Msg
import           Tt.Util

data WorkState = WorkState {
  currentStart :: Maybe (Project, LocalTime),
  entries      :: [Entry]
}

toWorkState :: [RawEntry] -> WorkState
toWorkState = foldl updateState (WorkState Nothing [])

-- | Close the current project with the current time to get work up to now
-- show up as session.
seal :: ZonedTime -> WorkState -> WorkState
seal _   (      WorkState Nothing             x) = WorkState Nothing x
seal now state@(WorkState (Just (project, _)) _) = foldl
  updateState
  state
  [ ClockOut day (time, Just tz)
  , ClockIn day (time, Just tz) project
  ]
 where
  lt   = zonedTimeToLocalTime now
  day  = localDay lt
  time = localTimeOfDay lt
  tz   = zonedTimeZone now

timeClocks :: WorkState -> [String]
timeClocks state = concat $ unfoldr unfoldTimeclock state
 where
  unfoldTimeclock (WorkState Nothing []) = Nothing
  unfoldTimeclock (WorkState (Just (p, t)) []) =
    Just ([Msg.timeclockIn t p], WorkState Nothing [])
  unfoldTimeclock (WorkState c (e:es)) =
    Just (handleEntry e, WorkState c es)
  handleEntry (SessionEntry p s) = clocksFor p (asTimeInterval s)
  handleEntry _                  = []
  clocksFor p i
    | intervalDuration i > 0
    = [Msg.timeclockIn (inf i) p, Msg.timeclockOut (sup i)]
    | otherwise
    = []

currentProject :: WorkState -> Maybe Project
currentProject state = fst <$> currentStart state

currentProjectStart :: WorkState -> Maybe LocalTime
currentProjectStart state = snd <$> currentStart state

plannedProject :: WorkState -> LocalTime -> Maybe (Project, Session)
plannedProject state t = listToMaybe $ mapMaybe f (entries state)
 where
  -- If the end of the session is in the future (check 1 second after now),
  -- show it as planned work.
  f (SessionEntry p s) | (tPlusOne `member` asTimeInterval s) && sessionHasTimeOfDay s
    = Just (p, s)
  f _ = Nothing
  tPlusOne = secondsToNominalDiffTime 1 `addLocalTime` t

updateState :: WorkState -> RawEntry -> WorkState
updateState state (ClockIn day (time, _) project) =
  open state project (LocalTime day time)
updateState state (ClockOut day (time, _)) =
  close state (LocalTime day time)
updateState state (CleanEntry e) =
  state { entries = entries state ++ [e] }

-- | Modify WorkState with a clock in entry
open :: WorkState -> Project -> LocalTime -> WorkState
open state p begin = state { currentStart = modify (currentStart state) }
 where
  modify Nothing  = Just (p, begin)
  modify (Just _) = trace "Unmatched clock in" Just (p, begin)

-- | Modify WorkState with a clock out entry
close :: WorkState -> LocalTime -> WorkState
close state end = case currentStart state of
  Just (p, begin) ->
    WorkState Nothing
      $  entries state
      ++ [SessionEntry p (Session begin (Add t) (Just Duration))]
    where t = fromIntegral ((truncate $ end `diffLocalTime` begin) :: Integer)
  Nothing -> trace "Unmatched clock out" state

duration :: [Session] -> NominalDiffTime
duration ss = sum $ map (intervalDuration . asTimeInterval) ss

during :: [Session] -> Interval LocalTime -> [Session]
during units s = mapMaybe (intersectWork s) units

-- | Transform work session to have new time inverval
imposeInterval :: Session -> Interval LocalTime -> Session
imposeInterval (Session _ (Add _) (Just Duration)) i = Session
  (inf i)
  (Add (fromIntegral (truncate (intervalDuration i) :: Integer)))
  (Just Duration)
imposeInterval (Session _ a u) i = Session (inf i) a u

intersectWork :: Interval LocalTime -> Session -> Maybe Session
intersectWork i session = case i `intersection` asTimeInterval session of
  Nothing -> Nothing
  Just s  -> Just $ imposeInterval session s

onProject :: WorkState -> String -> [Session]
onProject state name = mapMaybe grab (entries state)
 where
  grab (SessionEntry p s) | p == name = Just s
  grab _                  = Nothing

onCurrentProject :: WorkState -> [Session]
onCurrentProject state = maybe [] (onProject state) (currentProject state)
