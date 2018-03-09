module Tt.Goal (
  Goal(goalSlope, goalUnit, failureCount),
  goalValue,
  activeGoals,
  initGoal,
  updateGoal,
  updateGoalClock,
  failureTime,
  goalTarget
) where

import qualified Data.Map.Strict as Map
import           Data.Ratio
import           Data.Time
import           Tt.Entry
import           Tt.Util

type ActiveGoals = Map.Map Project Goal

data Goal = Goal {
  goalSlope       :: Rational,
  goalUnit        :: Maybe Unit,
  goalValue'      :: Value Rational,
  goalOffset      :: Rational,
  goalCurrentTime :: Rational, -- In days
  failureCount    :: Integer,
  goalStartTime   :: Rational  -- In days
} deriving (Eq, Show)

activeGoals :: [Entry] -> [(Project, Goal)]
activeGoals = Map.toList . foldl updateState Map.empty

initGoal :: Day -> Rational -> Maybe Unit -> Goal
initGoal begin slope unit = Goal slope unit mempty 0 b 0 b
  where b = fromIntegral (toModifiedJulianDay begin)

updateState :: ActiveGoals -> Entry -> ActiveGoals
updateState state (StartGoal day project slope unit) =
  Map.insert project (initGoal day slope unit) state
updateState state (EndGoal _ project) = Map.delete project state
updateState state (SessionEntry project session) =
  Map.update (\x -> Just (updateGoal x session)) project state

goalValue :: Goal -> Rational
goalValue goal = case goalValue' goal of
  Add a   -> a
  Set _ a -> a

goalBase :: Goal -> Rational
goalBase goal = case goalValue' goal of
  Add _   -> 0
  Set a _ -> a

goalChange :: Goal -> Rational
goalChange goal = case goalValue' goal of
  Add a   -> a
  Set a b -> b - a

updateGoal :: Goal -> Session -> Goal
updateGoal goal session = if unitsMatch goal session
  then goal' { goalValue' = goalValue' goal' `mappend` sessionAmount session }
  else goal
 where
  goal' = updateGoalClock goal (sessionTime session)
  unitsMatch g s = case (goalUnit g, sessionUnit s) of
    (Just (Named a), Just (Named b)) | a == b -> True
    -- Match no unit to any non-duration, because that's assumed to be comment
    -- text. Unfortunately duration unit will mean that the value has been
    -- transformed into seconds and the original is lost, so we'll need to
    -- abandon those.
    (Nothing       , Just Duration ) -> False
    (Just Duration , Just Duration ) -> True
    (Nothing       , _             ) -> True
    _                                -> False

-- | Update goal to the present time without registering new sessions.
--
-- This will accumulate any goal failures that would be caused by advancing
-- the time with no further work towards the goal.
updateGoalClock :: Goal -> LocalTime -> Goal
updateGoalClock goal t = catchUp (goal { goalCurrentTime = newDay })
 where
  newDay = timeToDay t
  catchUp g | failureDay g < goalCurrentTime g = catchUp $ addFailure g
  catchUp g = g

failureTime :: Goal -> LocalTime
failureTime = dayToTime . failureDay

-- | Day value where goal will fail with no further action
failureDay :: Goal -> Rational
failureDay goal = start + fromIntegral (ceiling ((y + a - b) / a))
 where
  start = goalStartTime goal
  a     = goalSlope goal
  b     = goalOffset goal
  y     = goalChange goal

-- | The value the goal should have now to be exactly on the expected slope
goalTarget :: Goal -> Rational
goalTarget goal = a * x + b
 where
  x = goalCurrentTime goal - goalStartTime goal
  a = goalSlope goal
  b = goalOffset goal + goalBase goal

-- | Convert library time value to Rational days used by Goal
timeToDay :: LocalTime -> Rational
timeToDay t = fromIntegral (toModifiedJulianDay day) + fraction
 where
  day         = localDay t
  dayFraction = t `diffLocalTime` LocalTime day midnight
  fraction =
    fromIntegral (truncate dayFraction) % fromIntegral (truncate nominalDay)

dayToTime :: Rational -> LocalTime
dayToTime day =
  dayFraction
    `addLocalTime` LocalTime (ModifiedJulianDay $ fromIntegral $ truncate day)
                             midnight
  where dayFraction = nominalDay * fromRational (snd $ properFraction day)

-- | When the goal fails, increment failure counter and adjust offset so that
-- it will take a week to hit the next failure.
addFailure :: Goal -> Goal
addFailure goal = goal { failureCount = failureCount goal + 1
                       , goalOffset   = goalOffset goal + bump
                       }
  where bump = -7 * goalSlope goal
