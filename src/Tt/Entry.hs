{-# LANGUAGE DeriveFunctor #-}

module Tt.Entry (
  Entry(ClockIn, ClockOut, SessionEntry, GoalEntry),
  Project,
  Value(Add, Set),
  Unit(Duration, Named),
  showUnit,
  Session(Session, sessionTime, sessionAmount, sessionUnit),
  Goal(Goal, goalSpan, goalName, goalTarget, goalUnit),
  parseEntry,
) where

import           Control.Monad
import           Data.Char
import           Data.Fixed                (Pico)
import           Data.Maybe
import           Data.Time
import           Numeric.Interval.NonEmpty
import           Text.Parsec
import           Text.Parsec.String        (Parser)
import           Text.Printf
import           Tt.Util

data Entry =
    ClockIn Day (TimeOfDay, Maybe TimeZone) Project
  | ClockOut Day (TimeOfDay, Maybe TimeZone)
  | SessionEntry Project Session
  | GoalEntry Goal
  deriving (Eq, Show)

-- | Identifier for a project that can be worked on
type Project = String

-- | Accumulation of work or setting the current work amount for a project
--
-- Set tracks both the starting baseline of a Value sequence and the current
-- value. Folded sequences with Set values can then be described as, say,
-- going from 93 kg to 81 kg instead of just ending up at 81 kg.
data Value a = Add a | Set a a deriving (Eq, Show, Functor)

instance Num a => Monoid (Value a) where
  mempty = Add 0
  (Add a) `mappend` (Add b) = Add (a + b)
  -- Calculate initial baseline from pre-existing addition
  (Add a) `mappend` (Set b c) = Set (b - a) c
  -- Update value but maintain original baseline
  (Set a b) `mappend` (Add c) = Set a (b + c)
  (Set a _) `mappend` (Set _ b) = Set a b

-- | Unit of the work done for the project
data Unit = Duration | Named String deriving (Eq, Show)

showUnit :: Rational -> Maybe Unit -> String
showUnit amount (Just Duration) | amount < 3600 =
  printf "%d min" (truncate (amount / 60) :: Integer)
showUnit amount (Just Duration) =
  printf "%.1f h" $ realToFrac amount / (3600 :: Double)
showUnit amount (Just (Named u)) = unwords [showRat amount, u]
showUnit amount Nothing          = showRat amount

-- | A single unit of work
data Session = Session {
  sessionTime   :: LocalTime,
  sessionAmount :: Value Rational,
  sessionUnit   :: Maybe Unit
} deriving (Eq, Show)

instance AsTimeInterval Session where
  asTimeInterval (Session t (Add n) (Just Duration)) =
    t ... (fromIntegral (truncate n :: Integer) `addLocalTime` t)
  asTimeInterval s = singleton (sessionTime s)

data Goal = Goal {
  goalSpan   :: Interval LocalTime,
  goalName   :: Project,
  goalTarget :: Rational,
  goalUnit   :: Maybe Unit
} deriving (Eq, Show)

instance AsTimeInterval Goal where
  asTimeInterval = goalSpan

-- | Try to parse a line of text into an Entry
parseEntry :: String -> Maybe Entry
parseEntry s = case parse entryParser "" s of
  Right e -> Just e
  Left  _ -> Nothing


entryParser :: Parser Entry
entryParser = try clockIn <|> try clockOut <|> try goal <|> try session
 where
  clockIn =
    ClockIn <$> donePrefix <*> tok zonedTime <* tok (string "s") <*> tok symbol
  clockOut = ClockOut <$> donePrefix <*> tok zonedTime <* tok (string "e")

  goal =
    GoalEntry
      <$> (buildGoal <$> goalPrefix <*> tok symbol <*> goalQuantity <*> goalDue)
   where
    goalPrefix = tok (string "x") *> tok date <* tok (string "GOAL")
    goalDue    = tok (string "due:" *> date)
    goalQuantity =
      try ((,) <$> tok number <*> optionMaybe (tok symbol))
        <|> (,)
        <$> tok number
        <*> pure Nothing
    buildGoal begin project (target, unitName) end = Goal
      (d1 begin ... d2 end)
      project
      (target * multiplier)
      unit
     where
      (multiplier, unit) = convertUnit unitName
      d1 dy = LocalTime dy midnight
      d2 dy = LocalTime (addDays 1 dy) midnight

  session :: Parser Entry
  session =
    buildSession
      <$> donePrefix
      <*> optionMaybe (tok zonedTime)
      <*> tok symbol
      <*> quantity
   where
    buildSession d time project q = SessionEntry
      project
      (Session localtime (fmap (* multiplier) amount) unit)
     where
      amount             = fst q
      (multiplier, unit) = convertUnit (snd q)
      localtime          = LocalTime d (maybe midday fst time)

quantity :: Parser (Value Rational, Maybe String)
quantity = fromMaybe (Add 1, Nothing)
  <$> optionMaybe ((,) <$> tok1 amount <*> optionMaybe (tok symbol))
  where amount = ((\x -> Set x x) <$> (string "= " *> number)) <|> (Add <$> number)

donePrefix :: Parser Day
donePrefix = tok (string "x") *> tok date

convertUnit :: Maybe String -> (Rational, Maybe Unit)
convertUnit (Just "min"    ) = (60, Just Duration)
convertUnit (Just "minutes") = (60, Just Duration)
convertUnit (Just "h"      ) = (60 * 60, Just Duration)
convertUnit (Just "hours"  ) = (60 * 60, Just Duration)
convertUnit (Just "days"   ) = (60 * 60 * 24, Just Duration)
convertUnit x                = (1, Named <$> x)

-- | Token parser, match inner and whitespace or eof after it
tok :: Parser a -> Parser a
tok inner = inner <* (eof <|> void (many1 space))

-- | Token parser that allows at most 1 trailing char
tok1 :: Parser a -> Parser a
tok1 inner = inner <* (eof <|> void space) -- At most one trailing space

date :: Parser Day
date = fromGregorian <$> year <* char '-' <*> month <* char '-' <*> day

zonedTime :: Parser (TimeOfDay, Maybe TimeZone)
zonedTime = (,) <$> timeOfDay <*> optionMaybe zoneOffset

-- NB: Numbers that start with + can also parse as symbols. If both parses are
-- valid, try parsing as number first.
symbol :: Parser String
symbol = (:) <$> firstChar <*> many restChars
 where
  firstChar = letter <|> oneOf "_+@"
  restChars = letter <|> digit <|> oneOf "_+@-"

timeOfDay :: Parser TimeOfDay
timeOfDay =
  TimeOfDay <$> hour <* char ':' <*> minute <*> option 0 (char ':' *> second)

zoneOffset :: Parser TimeZone
zoneOffset = negativeZone <|> positiveZone
 where
  negativeZone  = (minutesToTimeZone . negate) <$> (char '-' *> hhmmToMinutes)
  positiveZone  = minutesToTimeZone <$> (char '+' *> hhmmToMinutes)
  hhmmToMinutes = (\h m -> h * 60 + m) <$> hour <*> minute

year :: Parser Integer
year = read <$> count 4 digit

month :: Parser Int
month = fromIntegral <$> belowHundred 1

day :: Parser Int
day = fromIntegral <$> belowHundred 3

hour :: Parser Int
hour = fromIntegral <$> belowHundred 2

minute :: Parser Int
minute = fromIntegral <$> belowHundred 5

second :: Parser Pico
second = fromIntegral <$> belowHundred 5

-- | Match two-digit numbers up to n * 10 + 9.
belowHundred :: Int -> Parser Integer
belowHundred n =
  read <$> ((:) <$> oneOf ['0' .. intToDigit n] <*> count 1 digit)

number :: Parser Rational
number =
  (negate <$> (char '-' *> decimal)) <|> (char '+' *> decimal) <|> decimal

decimal :: Parser Rational
decimal =
  try ((+) <$> (fromIntegral <$> integer) <*> fraction)
    <|> (fromIntegral <$> integer)

fraction :: Parser Rational
fraction =
  (\x -> fromIntegral (read x :: Integer) / (10 ^ length x))
    <$> (char '.' *> many1 digit)

integer :: Parser Integer
integer = read <$> many1 digit


--token :: Parser Token
