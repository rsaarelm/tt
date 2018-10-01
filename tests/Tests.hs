module Main where

import           Data.Fixed
import           Data.Time
import           Test.Hspec
import           Tt.Entry
import           Tt.Goal
import           Tt.Parser
import           Tt.Util

main :: IO ()
main = hspec $ do
  describe "Goal tracking" $ do
    it "computes initial goal value" $
      goalValue simpleGoal `shouldBe` 0
    it "updates goal with incremental value" $
      goalValue (simpleGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) Nothing)
      `shouldBe` 4
    it "updates goal with absolute value" $
      goalValue (simpleGoal `updateGoal`
        Session (ld 2018 01 02) (Set 52 52) Nothing)
      `shouldBe` 52
    it "fails a goal after a day of falling below the target line" $
      failureTime simpleGoal
      `shouldBe` LocalTime (fromGregorian 2018 1 3) midnight
    it "fails a sloped goal after a day of falling below the target line" $
      failureTime (initGoal (fromGregorian 2018 1 1) 999 Nothing)
      `shouldBe` LocalTime (fromGregorian 2018 1 3) midnight
    it "gives you a week of extra time after goal failure" $
      failureTime (simpleGoal `updateGoalClock` ld 2018 1 4)
      `shouldBe` LocalTime (fromGregorian 2018 1 10) midnight
    it "works for negative slopes" $
      failureTime (negativeGoal `updateGoalClock` ld 2018 1 4)
      `shouldBe` LocalTime (fromGregorian 2018 1 10) midnight
    it "handles generating multiple failures at once" $
      failureTime (simpleGoal `updateGoalClock` ld 2018 1 31)
      `shouldBe` LocalTime (fromGregorian 2018 2 7) midnight
    it "counts multiple failures correctly" $
      failureCount (simpleGoal `updateGoalClock` ld 2018 1 31)
      `shouldBe` 5
    it "counts multiple failures for negative goal correctly" $
      failureCount (negativeGoal `updateGoalClock` ld 2018 1 31)
      `shouldBe` 5
    it "accepts random units for unitless goal" $
      goalValue (simpleGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) (Just (Named "the")))
      `shouldBe` 4
    it "does not accept duration units for unitless goal" $
      goalValue (simpleGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) (Just Duration))
      `shouldBe` 0
    it "rejects unitless session for goal with unit" $
      goalValue (kmGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) Nothing)
      `shouldBe` 0
    it "accepts matching unit session for goal with unit" $
      goalValue (kmGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) (Just (Named "km")))
      `shouldBe` 4
    it "rejects mismatching unit session for goal with unit" $
      goalValue (kmGoal `updateGoal`
        Session (ld 2018 01 03) (Add 4) (Just (Named "the")))
      `shouldBe` 0
    it "accepts duration units for duration goal" $
      goalValue (durationGoal `updateGoal`
        Session (ld 2018 01 03) (Add 60) (Just Duration))
      `shouldBe` 60
    it "rejects unitless session for duration goal" $
      goalValue (durationGoal `updateGoal`
        Session (ld 2018 01 03) (Add 60) Nothing)
      `shouldBe` 0
    it "rejects named unit for duration goal" $
      goalValue (durationGoal `updateGoal`
        Session (ld 2018 01 03) (Add 60) (Just (Named "the")))
      `shouldBe` 0

  describe "Entry parser" $ do
    it "fails to parse empty strings" $ "" `shouldNotParse` ()
    it "fails to parse todo entries" $
      "2018-02-01 Do a stuff" `shouldNotParse` ()
    it "fails to parse priorized todo entries" $
      "(x) 2018-02-01 Do a stuff" `shouldNotParse` ()
    it "fails to parse regular done entries" $
      "x 2018-02-01 2018-02-10 Do a stuff" `shouldNotParse` ()

    it "parses clocking in" $
      "x 2018-02-10 10:29:22 s quux random stuff" `shouldParseAs`
        ClockIn (d 2018 02 10) (t 10 29 22 (-1)) "quux"
    it "parses clocking in with time zone" $
      "x 2018-02-10 10:29:22+0200 s quux random stuff" `shouldParseAs`
        ClockIn (d 2018 02 10) (t 10 29 22 120) "quux"
    it "parses clocking out" $
      "x 2018-02-10 10:29:22 e" `shouldParseAs`
        ClockOut (d 2018 02 10) (t 10 29 22 (-1))
    it "fails to parse clocking in without a project" $
      "x 2018-02-10 10:29:22+0200 s   " `shouldNotParse` ()
    it "fails to parse clocking in without a time of day" $
      "x 2018-02-10 s quux" `shouldNotParse` ()

    it "parses simple accumulation" $
      "x 2018-02-10 thing" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 1) Nothing)
    it "parses simple accumulation with time" $
      "x 2018-02-10 11:34 thing" `shouldParseAsSession`
        ("thing", Session (ldt 2018 02 10 11 34 0) (Add 1) Nothing)
    it "does not care about stuff after simple accumulation" $
      "x 2018-02-10 thing qux quux" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 1) Nothing)
    it "does not care about comment after simple accumulation" $
      "x 2018-02-10 thing  -- Comment thing" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 1) Nothing)
    it "parses quantified accumulation" $
      "x 2018-02-10 thing 12" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 12) Nothing)
    it "requires quantity to parse unit" $
      "x 2018-02-10 thing km" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 1) Nothing)
    it "parser accumulation with unit and quantity" $
      "x 2018-02-10 run 14 km" `shouldParseAsSession`
        ("run", Session (ld 2018 02 10) (Add 14) (Just $ Named "km"))
    it "does not parse comment as unit if you separate it with two spaces" $
      "x 2018-02-10 thing 12  The 'The' in this comment is not an unit!" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 12) Nothing)
    it "parses decimal quantity accumulation" $
      "x 2018-02-10 thing 12.34" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Add 12.34) Nothing)
    it "parses setting the quantity" $
      "x 2018-02-10 thing = 27" `shouldParseAsSession`
        ("thing", Session (ld 2018 02 10) (Set 27 27) Nothing)

    -- It's important to support time of day in absolute setting, because the
    -- user may sort the todo.txt file and multiple absolute values during the
    -- same day may end up in the wrong order if there's no extra sort data
    -- imposed.
    it "parses setting the quantity with time of day" $
      "x 2018-02-10 09:30 thing = 29" `shouldParseAsSession`
        ("thing", Session (ldt 2018 02 10 9 30 0) (Set 29 29) Nothing)

    it "parses setting the with unit" $
      "x 2018-02-10 odometer = 128 km" `shouldParseAsSession`
        ("odometer", Session (ld 2018 02 10) (Set 128 128) (Just $ Named "km"))
    it "parses minutes as duration" $
      "x 2018-02-10 job 10 min" `shouldParseAsSession`
        ("job", Session (ld 2018 02 10) (Add 600) (Just Duration))
    it "parses time in session entries" $
      "x 2018-02-10 11:34 job 10 min"
      `shouldParseAsSession`
      ("job", Session (ldt 2018 02 10 11 34 0) (Add 600) (Just Duration))
    it "parses hours as duration" $
      "x 2018-02-10 11:34 job 2 h"
      `shouldParseAsSession`
      ("job", Session (ldt 2018 02 10 11 34 0) (Add 7200) (Just Duration))

    it "parses simple goal" $
      "x 2018-02-10 GOAL floss 7" `shouldParseEntry`
        StartGoal (d 2018 2 10) "floss" 1 Nothing
    it "parses simple goal with a comment" $
      "x 2018-02-10 GOAL floss 7  -- Do this thing" `shouldParseEntry`
        StartGoal (d 2018 2 10) "floss" 1 Nothing
    it "does not parse a goal with zero slope" $
      "x 2018-02-10 GOAL floss 0" `shouldNotParse` ()
    it "parses a goal with units" $
      "x 2018-02-10 GOAL run 14 km" `shouldParseEntry`
        StartGoal (d 2018 2 10) "run" 2 (Just $ Named "km")
    it "parses a goal with units and comment" $
      "x 2018-02-10 GOAL run 14 km  -- Comment" `shouldParseEntry`
        StartGoal (d 2018 2 10) "run" 2 (Just $ Named "km")
    it "parses a goal with time units (hours)" $
      "x 2018-02-10 GOAL read 7 h" `shouldParseEntry`
        StartGoal (d 2018 2 10) "read" 3600 (Just Duration)
    it "parses a goal with time units (minutes)" $
      "x 2018-02-10 GOAL flight-training 210 min"
      `shouldParseEntry`
      StartGoal (d 2018 2 10) "flight-training" 1800 (Just Duration)
    it "parses dropping a goal" $
      "x 2018-03-10 DROP GOAL floss" `shouldParseEntry`
        EndGoal (d 2018 3 10) "floss"

  describe "Relative time parser" $ do
    it "parsers boot time" $
      "boot" `timeShouldParse` SinceSystemStartup
    it "parser absolute time" $
      "07:15" `timeShouldParse` AbsoluteTime (TimeOfDay 7 15 0)
    it "parser absolute time with seconds" $
      "07:15:30" `timeShouldParse` AbsoluteTime (TimeOfDay 7 15 30)
    it "parser absolute time without leading zero" $
      "7:15" `timeShouldParse` AbsoluteTime (TimeOfDay 7 15 0)
    it "parser 24h absolute time" $
      "19:15" `timeShouldParse` AbsoluteTime (TimeOfDay 19 15 0)
    it "parser relative time in hours" $
      "in 5 h" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (3600 * 5))
    it "parser relative time without space" $
      "in 5h" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (3600 * 5))
    it "parser negative relative time" $
      "in -5 h" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (3600 * (-5)))
    it "parser fractional relative time" $
      "in 5.5 h" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (3600 * 5 + 1800))
    it "parser retroactive relative time in hours" $
      "5 h ago" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (3600 * (-5)))
    it "parser total time" $
      "after 8 h" `timeShouldParse` AfterTotalTime (secondsToNominalDiffTime (3600 * 8))
    it "parser relative time in minutes" $
      "in 30 min" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (60 * 30))
    it "parser relative time in minutes without space" $
      "in 30min" `timeShouldParse` RelativeTime (secondsToNominalDiffTime (60 * 30))

-- Utility functions

shouldNotParse :: String -> () -> Expectation
shouldNotParse s _ = parseEntry s `shouldBe` Nothing

shouldParseAs :: String -> RawEntry -> Expectation
shouldParseAs s expected = parseEntry s `shouldBe` Just expected

shouldParseAsSession :: String -> (String, Session) -> Expectation
shouldParseAsSession s (expectedName, expected) =
  parseEntry s `shouldBe` Just (CleanEntry $ SessionEntry expectedName expected)

shouldParseEntry :: String -> Entry -> Expectation
shouldParseEntry s expected =
  parseEntry s `shouldBe` Just (CleanEntry expected)

timeShouldParse :: String -> TimeExpr -> Expectation
timeShouldParse s expected = parseTimeExpr s `shouldBe` Just expected

timeShouldNotParse :: String -> () -> Expectation
timeShouldNotParse s _ = parseTimeExpr s `shouldBe` Nothing

d :: Integer -> Int -> Int -> Day
d = fromGregorian

ld :: Integer -> Int -> Int -> LocalTime
ld y m day = LocalTime (fromGregorian y m day) midday

gd :: Integer -> Int -> Int -> LocalTime
gd y m day = LocalTime (fromGregorian y m day) midnight

ldt :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
ldt y m day h mn sec = LocalTime (fromGregorian y m day) (TimeOfDay h mn sec)

simpleGoal :: Goal
simpleGoal = initGoal (fromGregorian 2018 1 1) 1 Nothing

negativeGoal :: Goal
negativeGoal = initGoal (fromGregorian 2018 1 1) (-1) Nothing

kmGoal :: Goal
kmGoal = initGoal (fromGregorian 2018 1 1) 10 (Just (Named "km"))

durationGoal :: Goal
durationGoal = initGoal (fromGregorian 2018 1 1) 3600 (Just Duration)

t :: Int -> Int -> Pico -> Int -> (TimeOfDay, Maybe TimeZone)
t h m s (-1)  = (TimeOfDay h m s, Nothing)
t h m s tzMin = (TimeOfDay h m s, Just (minutesToTimeZone tzMin))
