module Tt.Work (
  WorkState(projectSessions),
  toWorkState,
  seal,
  timeClocks,
  currentProject,
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
  currentStart    :: Maybe (Project, LocalTime),
  projectSessions :: [(Project, Session)]
}

toWorkState :: [Entry] -> WorkState
toWorkState = foldl updateState (WorkState Nothing [])

-- | Close the current project with the current time to get work up to now
-- show up as session.
seal :: ZonedTime -> WorkState -> WorkState
seal _   (      WorkState Nothing                 x ) = WorkState Nothing x
seal now state@(WorkState (Just (project, _)) _) = foldl
  updateState
  state
  [ClockOut day (time, Just tz), ClockIn day (time, Just tz) project]
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
  unfoldTimeclock (WorkState c ((p, s):ss)) =
    Just (clocksFor p (asTimeInterval s), WorkState c ss)
  clocksFor p i
    | intervalDuration i > 0
    = [Msg.timeclockIn (inf i) p, Msg.timeclockOut (sup i)]
    | otherwise
    = []

currentProject :: WorkState -> Maybe Project
currentProject state = fst <$> currentStart state

updateState :: WorkState -> Entry -> WorkState
updateState state (ClockIn day (time, _) project) =
  open state project (LocalTime day time)
updateState state (ClockOut day (time, _)) = close state (LocalTime day time)
updateState state (SessionEntry project session) =
  state { projectSessions = projectSessions state ++ [(project, session)] }
updateState state (GoalEntry _) = state
updateState state (EndGoal _ _) = state

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
      $  projectSessions state
      ++ [(p, Session begin (Add t) (Just Duration))]
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
onProject state name =
  map snd $ filter ((name ==) . fst) (projectSessions state)

onCurrentProject :: WorkState -> [Session]
onCurrentProject state = maybe [] (onProject state) (currentProject state)
