{-# LANGUAGE DeriveFunctor #-}

module Tt.Entry (
  Entry(SessionEntry, PlannedSession, StartGoal, EndGoal),
  RawEntry(ClockIn, ClockOut, CleanEntry),
  entrySortKey,
  Project,
  Value(Add, Set),
  Unit(Duration, Named),
  showUnit,
  Session(Session, sessionTime, sessionAmount, sessionUnit),
  parseEntry,
) where

import           Control.Monad
import           Data.Char
import           Data.Fixed                (Pico)
import           Data.Time
import           Numeric.Interval.NonEmpty
import           Text.Parsec
import           Text.Parsec.String        (Parser)
import           Text.Printf
import           Tt.Util

-- | Entry data the higher application layers consume
--
-- Clock values from raw entries are converted into SessionEntries.
data Entry =
    SessionEntry Project Session
  | PlannedSession Project Session
  | StartGoal Day Project Rational (Maybe Unit)
  | EndGoal Day Project
  deriving (Eq, Show)

-- | Unsanitized entries with clock values.
data RawEntry =
    ClockIn Day (TimeOfDay, Maybe TimeZone) Project
  | ClockOut Day (TimeOfDay, Maybe TimeZone)
  | CleanEntry Entry
  deriving (Eq, Show)

-- | Sort key for the entries
--
-- The basic sorting is based on the timestamp of the entry, a secondary
-- parameter is provided to control sorting of entries that land on the same
-- timestamp. It's possible to get clock out and clock in entries at the same
-- timestamp, and the clock in should then be put after the clock out.
entrySortKey :: RawEntry -> (LocalTime, Int)
entrySortKey (ClockIn d (t, _) _) = (LocalTime d t, 1)
entrySortKey (ClockOut d (t, _)) = (LocalTime d t, 0)
entrySortKey (CleanEntry (SessionEntry _ s)) = (inf $ asTimeInterval s, 0)
entrySortKey (CleanEntry (PlannedSession _ s)) = (inf $ asTimeInterval s, 0)
entrySortKey (CleanEntry (StartGoal d _ _ _)) = (LocalTime d midnight, 0)
entrySortKey (CleanEntry (EndGoal d _)) = (LocalTime (addDays 1 d) midnight, 0)

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
  printf "%s h" (showRat (amount / 3600))
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

-- | Try to parse a line of text into an Entry
parseEntry :: String -> Maybe RawEntry
parseEntry s = case parse entryParser "" s of
  Right e -> Just e
  Left  _ -> Nothing


-- | Input entry main parser
entryParser :: Parser RawEntry
entryParser =
  try clockIn <|> try clockOut <|> try goal <|> try endGoal <|> try session
    <|> try plannedSession
 where
  clockIn = ClockIn <$> donePrefix <*> tok zonedTime <*  tok (string "s") <*> tok projectName
  clockOut = ClockOut <$> donePrefix <*> tok zonedTime <* tok (string "e")

  goal =
    CleanEntry
      <$> (   buildGoal
          <$> goalPrefix
          <*> tok projectName
          <*> tok nonzero
          <*> optionMaybe (tok symbol)
          )
   where
    goalPrefix = tok (string "x") *> tok date <* tok (string "GOAL")
    buildGoal :: Day -> String -> Rational -> Maybe String -> Entry
    buildGoal begin project slope inputUnit = StartGoal begin
                                                        project
                                                        (slope * multiplier / 7)
                                                        unit
      where (multiplier, unit) = convertUnit inputUnit

  endGoal = CleanEntry <$> (EndGoal <$> endGoalPrefix <*> tok projectName)
   where
    endGoalPrefix = tok (string "x") *> tok date <* tok (string "DROP GOAL")

  session =
    CleanEntry
      <$> (   buildSession
          <$> donePrefix
          <*> optionMaybe (tok zonedTime)
          <*> tok projectName
          <*> (try quantity <|> return (Add 1, Nothing))
          )
   where
    buildSession d time project (amount, inputUnit) = SessionEntry
      project
      (Session localtime (fmap (* multiplier) amount) unit)
     where
      (multiplier, unit) = convertUnit inputUnit
      localtime          = LocalTime d (maybe midday fst time)

  -- Planned entries must have duration type and include a time of day, parser
  -- is a bit different from regular session.
  plannedSession =
    CleanEntry
      <$> (   buildSession
          <$> tok date
          <*> tok zonedTime
          <*> tok projectName
          <*> duration
          )
   where
    buildSession d time project seconds = PlannedSession
      project
      (Session localtime (Add seconds) (Just Duration))
     where
      localtime          = LocalTime d (fst time)

-- | Parse a relative or absolute quantity with an optional unit.
quantity :: Parser (Value Rational, Maybe String)
quantity = (,) <$> tok1 amount <*> optionMaybe (tok symbol)
 where
  amount = ((\x -> Set x x) <$> (string "= " *> number)) <|> (Add <$> number)

-- | Parse a duration quantity. Return duration converted to seconds.
duration :: Parser Rational
duration = do
  amount <- tok1 number
  unit <- tok symbol
  case convertUnit (Just unit) of
    (n, Just Duration) -> return (amount * n)
    _                  -> fail "not a duration"

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

-- Like symbol, but exclude reserved words
projectName :: Parser String
projectName = do
  sym <- symbol
  case sym of
    "GOAL" -> fail "reserved word"
    "s"    -> fail "reserved word"
    "e"    -> fail "reserved word"
    s      -> return s

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

nonzero :: Parser Rational
nonzero = do
  num <- number
  if num == 0 then fail "" else return num

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
