module Tt.Goal (
  Goal(goalBegin, goalName, goalTarget, goalUnit, goalEnd),
  toGoal,
  activeGoals,
  progressStats,
) where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Tt.Db
import           Tt.Token

data Goal = Goal {
  goalBegin  :: Day,
  goalName   :: String,
  goalTarget :: Rational,
  goalUnit   :: Maybe String,
  goalEnd    :: Day
} deriving (Show)

data Value a =
    Add a
  | Set a
    deriving (Show)

addValue :: Num a => a -> Value a -> a
addValue _ (Set b) = b
addValue a (Add b) = a + b

data DataPoint = DataPoint {
   dataPointDay    :: Day,
   dataPointGoal   :: String,
   dataPointAmount :: Value Rational,
   dataPointUnit   :: Maybe String
} deriving (Show)

toGoal :: Entry -> Maybe Goal
toGoal (Sym "x":Date begin:Sym "GOAL":Sym name:Number target:Colon (Sym "due") (Date end):_)
  = Just $ Goal begin name target Nothing end
toGoal (Sym "x":Date begin:Sym "GOAL":Sym name:Number target:Sym unit:Colon (Sym "due") (Date end):_)
  = Just $ Goal begin name target (Just unit) end
toGoal _ = Nothing

toDataPoint :: Entry -> Maybe DataPoint
-- NB: DataPoints may get some random word as unit, they must only match
-- Goal's unit if Goal's unit isn't Nothing.
toDataPoint (Sym "x":Date day:Sym goal:Number amount:es) =
  Just $ DataPoint day goal (Add amount) (parseUnit es)
toDataPoint (Sym "x":Date day:Sym goal:Text "=":Number amount:es) =
  Just $ DataPoint day goal (Set amount) (parseUnit es)
toDataPoint (Sym "x":Date day:Sym goal:_) =
  Just $ DataPoint day goal (Add 1) Nothing
toDataPoint _ = Nothing

parseUnit :: [Token] -> Maybe String
parseUnit (Sym unit:_) = Just unit
parseUnit _            = Nothing

belongsIn :: Goal -> DataPoint -> Bool
belongsIn goal point =
  goalMatches && unitMatches && pointDay >= minDay && pointDay <= maxDay
 where
  minDay = toModifiedJulianDay (goalBegin goal)
  maxDay = toModifiedJulianDay (goalEnd goal)
  unitMatches =
    isNothing (goalUnit goal) || dataPointUnit point == goalUnit goal
  goalMatches = goalName goal == dataPointGoal point
  pointDay    = toModifiedJulianDay (dataPointDay point)

containsDay :: Day -> Goal -> Bool
containsDay day goal = day >= goalBegin goal && day <= goalEnd goal

dataPoints :: Goal -> Db -> [DataPoint]
dataPoints goal db = sortBy (\a b -> dataPointDay a `compare` dataPointDay b)
                            points
  where points = filter (belongsIn goal) $ mapMaybe toDataPoint db

activeGoals :: Db -> Day -> [Goal]
activeGoals db day = filter (containsDay day) $ mapMaybe toGoal db

-- | Return how far along you are and how many days you are ahead.
progressStats :: Db -> Day -> Goal -> (Rational, Rational)
progressStats db day goal = (current, progressDay - absoluteDay)
 where
  points      = dataPoints goal db
  nDays       = fromIntegral (goalEnd goal `diffDays` goalBegin goal) + 1
  absoluteDay = fromIntegral (day `diffDays` goalBegin goal) -- + (1 % 2)
  start       = startValue points
  current     = pointValue points
  end         = goalTarget goal
  progressDay =
    if end /= start then nDays * (current - start) / (end - start) else 0

startValue :: [DataPoint] -> Rational
startValue ps = extract $ map dataPointAmount ps
 where
  extract (Set x:_) = x
  extract _         = 0

pointValue :: [DataPoint] -> Rational
pointValue ps = foldl addValue 0 (map dataPointAmount ps)
