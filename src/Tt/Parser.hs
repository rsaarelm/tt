module Tt.Parser (
  parseEntry,
  TimeExpr(AbsoluteTime, RelativeTime, AfterTotalTime, SinceSystemStartup),
  parseTimeExpr,
) where

import           Control.Monad
import           Data.Char
import           Data.Fixed                (Pico)
import           Data.Time
import           Text.Parsec
import           Text.Parsec.String        (Parser)
import           Tt.Entry
import           Tt.Util

data TimeExpr =
    AbsoluteTime TimeOfDay
  | RelativeTime NominalDiffTime
  | AfterTotalTime NominalDiffTime
  | SinceSystemStartup
  deriving (Eq, Show)

-- | Try to parse a line of thext into a timeExpr
parseTimeExpr :: String -> Maybe TimeExpr
parseTimeExpr s = case parse timeExprParser "" s of
  Right e -> Just e
  Left  _ -> Nothing

timeExprParser :: Parser TimeExpr
timeExprParser =
      try absoluteTime
  <|> try relativeTime
  <|> try negativeRelativeTime
  <|> try afterTotalTime
  <|> try sinceStartup
 where
  absoluteTime = AbsoluteTime <$> timeOfDay
  relativeTime = RelativeTime <$> (tok (string "in") *> diffTime)
  negativeRelativeTime = RelativeTime . negate <$> diffTime <* many1 space <* string "ago"
  afterTotalTime = AfterTotalTime <$> (tok (string "after") *> diffTime)
  sinceStartup = const SinceSystemStartup <$> tok (string "boot")

diffTime :: Parser NominalDiffTime
diffTime =
  secondsToNominalDiffTime . fromIntegral . truncate <$>
    (try ((* 60) <$> parseMinutes) <|> try ((* 3600) <$> parseHours))
 where
  parseMinutes = number <* skipMany space <* string "min"
  parseHours = number <* skipMany space <* string "h"


-- | Try to parse a line of text into an Entry
parseEntry :: String -> Maybe RawEntry
parseEntry s = case parse entryParser "" s of
  Right e -> Just e
  Left  _ -> Nothing

-- | Input entry main parser
entryParser :: Parser RawEntry
entryParser =
  try clockIn <|> try clockOut <|> try goal <|> try endGoal <|> try session
 where
  clockIn = ClockIn <$> donePrefix <*> tok zonedTime <*  tok (string "s") <*> tok projectName
  clockOut = ClockOut <$> donePrefix <*> tok zonedTime <* tok (string "e")

  goal =
    CleanEntry
      <$> (   buildGoal
          <$> goalPrefix
          <*> tok projectName
          <*> (try mixedTimeGoalTarget <|> goalTarget)
          )
   where
    goalPrefix = tok (string "x") *> tok date <* tok (string "GOAL")
    buildGoal :: Day -> String -> (Rational, Maybe Unit) -> Entry
    buildGoal begin project (slope, unit) = StartGoal begin
                                                      project
                                                      (slope / 7)
                                                      unit
    mixedTimeGoalTarget = f <$> tok nonzero <* string "h " <*> tok1 number <* string "min"
     where
      f h m = (h * 3600 + m * 60, Just Duration)
    goalTarget = f <$> tok nonzero <*> optionMaybe (tok symbol)
     where
      f x u = (x * multiplier, u')
       where (multiplier, u') = convertUnit u

  endGoal = CleanEntry <$> (EndGoal <$> endGoalPrefix <*> tok projectName)
   where
    endGoalPrefix = tok (string "x") *> tok date <* tok (string "DROP GOAL")

  session =
    CleanEntry
      <$> (   buildSession
          <$> donePrefix
          <*> optionMaybe (tok zonedTime)
          <*> tok projectName
          <*> (try mixedTimeQuantity <|> try quantity <|> return (Add 1, Nothing))
          )
   where
    buildSession d time project (amount, inputUnit) = SessionEntry
      project
      (Session localtime (fmap (* multiplier) amount) unit)
     where
      (multiplier, unit) = convertUnit inputUnit
      localtime          = LocalTime d (maybe midday fst time)

-- | Parse a relative or absolute quantity with an optional unit.
quantity :: Parser (Value Rational, Maybe String)
quantity = (,) <$> tok1 amount <*> optionMaybe (tok symbol)
 where
  amount = ((\x -> Set x x) <$> (string "= " *> number)) <|> (Add <$> number)

-- | Parse hours and minutes mixed quantity
mixedTimeQuantity :: Parser (Value Rational, Maybe String)
mixedTimeQuantity = mergeHMin <$>
  tok1 number <* string "h " <*> tok1 number <* string "min"
 where
  mergeHMin h m = (Add (60 * h + m), Just "min")

donePrefix :: Parser Day
donePrefix = tok (string "x") *> tok date

convertUnit :: Maybe String -> (Rational, Maybe Unit)
convertUnit (Just "min"    ) = (60, Just Duration)
convertUnit (Just "h"      ) = (60 * 60, Just Duration)
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
  negativeZone  = minutesToTimeZone . negate <$> (char '-' *> hhmmToMinutes)
  positiveZone  = minutesToTimeZone <$> (char '+' *> hhmmToMinutes)
  hhmmToMinutes = (\h m -> h * 60 + m) <$> hour <*> minute

year :: Parser Integer
year = read <$> count 4 digit

month :: Parser Int
month = fromIntegral <$> belowHundred 1

day :: Parser Int
day = fromIntegral <$> belowHundred 3

hour :: Parser Int
hour = fromIntegral <$> belowHundred 2 <|> read <$> count 1 digit

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
