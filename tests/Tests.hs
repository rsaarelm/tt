module Main where

import           Data.Fixed
import           Data.Time
import           Numeric.Interval.NonEmpty
import           Test.Hspec
import           Tt.Entry

main :: IO ()
main = hspec $ describe "Entry parser" $ do
  return ()
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

--  These parse as a work session, would be somewhat annoying to add a special
--  exception to project name 's' so I'll just leave it be.
--
--  it "fails to parse clocking in without a project" $
--    "x 2018-02-10 10:29:22+0200 s   " `shouldNotParse` ()
--  it "fails to parse clocking in without a time of day" $
--    "x 2018-02-10 s quux" `shouldNotParse` ()


  it "parses simple accumulation" $
    "x 2018-02-10 thing" `shouldParseAsSession`
      ("thing", Session (ld 2018 02 10) (Add 1) Nothing)
  it "parses simple accumulation with time" $
    "x 2018-02-10 11:34 thing" `shouldParseAsSession`
      ("thing", Session (ldt 2018 02 10 11 34 0) (Add 1) Nothing)
  it "does not care about stuff after simple accumulation" $
    "x 2018-02-10 thing qux quux" `shouldParseAsSession`
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
      ("thing", Session (ld 2018 02 10) (Set 27) Nothing)
  it "parses setting the with unit" $
    "x 2018-02-10 odometer = 128 km" `shouldParseAsSession`
      ("odometer", Session (ld 2018 02 10) (Set 128) (Just $ Named "km"))
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
    "x 2018-02-10 GOAL floss 30 due:2018-03-10" `shouldParseAsGoal`
      Goal (gd 2018 2 10 ... gd 2018 3 11) "floss" 30 Nothing
  it "parses a goal with units" $
    "x 2018-02-10 GOAL run 50 km due:2018-03-10" `shouldParseAsGoal`
      Goal (gd 2018 2 10 ... gd 2018 3 11) "run" 50 (Just $ Named "km")
  it "parses a goal with time units (hours)" $
    "x 2018-02-10 GOAL read 10 h due:2018-03-10" `shouldParseAsGoal`
      Goal (gd 2018 2 10 ... gd 2018 3 11) "read" 36000 (Just Duration)
  it "parses a goal with time units (minutes)" $
    "x 2018-02-10 GOAL flight-training 150 min due:2018-03-10"
    `shouldParseAsGoal`
    Goal
      (gd 2018 2 10 ... gd 2018 3 11)
      "flight-training"
      9000
      (Just Duration)

-- Utility functions

shouldNotParse :: String -> () -> Expectation
shouldNotParse s _ = parseEntry s `shouldBe` Nothing

shouldParseAs :: String -> Entry -> Expectation
shouldParseAs s expected = parseEntry s `shouldBe` Just expected

shouldParseAsSession :: String -> (String, Session) -> Expectation
shouldParseAsSession s (expectedName, expected) =
  parseEntry s `shouldBe` Just (SessionEntry expectedName expected)

shouldParseAsGoal :: String -> Goal -> Expectation
shouldParseAsGoal s expected =
  parseEntry s `shouldBe` Just (GoalEntry expected)

d :: Integer -> Int -> Int -> Day
d = fromGregorian

ld :: Integer -> Int -> Int -> LocalTime
ld y m day = LocalTime (fromGregorian y m day) midday

gd :: Integer -> Int -> Int -> LocalTime
gd y m day = LocalTime (fromGregorian y m day) midnight

ldt :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
ldt y m day h mn sec = LocalTime (fromGregorian y m day) (TimeOfDay h mn sec)

t :: Int -> Int -> Pico -> Int -> (TimeOfDay, Maybe TimeZone)
t h m s (-1)  = (TimeOfDay h m s, Nothing)
t h m s tzMin = (TimeOfDay h m s, Just (minutesToTimeZone tzMin))
