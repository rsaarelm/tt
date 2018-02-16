module Tt.Goal (
  activeGoals,
  towards,
  daysSpanned,
  goalStart,
  goalDays,
  totalWork
) where

import           Data.Maybe
import           Data.Time
import           Numeric.Interval.NonEmpty
import           Tt.Entry
import           Tt.Util
import           Tt.Work

activeGoals :: ZonedTime -> [Entry] -> [Goal]
activeGoals now = mapMaybe (match (zonedTimeToLocalTime now))
 where
  match t (GoalEntry g) | t `member` asTimeInterval g = Just g
  match _ _             = Nothing

-- | How much of the goal's available time has been spanned now.
daysSpanned :: Goal -> ZonedTime -> Rational
daysSpanned goal zt = goalDays goal * d (inf gs ... lt) / d gs
 where
  gs = asTimeInterval goal
  lt = zonedTimeToLocalTime zt
  d i = fromIntegral (truncate (intervalDuration i) :: Integer)

towards :: WorkState -> Goal -> [Session]
towards state g = mapMaybe match (projectSessions state)
 where
  match (p, s) | p == goalName g && sessionUnit s `matchesUnit` goalUnit g =
    asTimeInterval g `intersectWork` s
  match _ = Nothing
  matchesUnit _        Nothing  = True  -- Anything goes if goal has no unit
  matchesUnit (Just a) (Just b) | a == b = True
  matchesUnit _        _        = False

-- | What's the starting value for the goal.
--
-- Zero, except if the first value in the work is a Set Value.
goalStart :: [Session] -> Rational
goalStart work = case mconcat (map sessionAmount work) of
  Add _   -> 0
  Set x _ -> x

goalDays :: Goal -> Rational
goalDays goal = fromIntegral (truncate days :: Integer)
  where days = intervalDuration (asTimeInterval goal) / (60 * 60 * 24)

totalWork :: [Session] -> Rational
totalWork work = case mconcat (map sessionAmount work) of
  Add x   -> x
  Set _ x -> x
