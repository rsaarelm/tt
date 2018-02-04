module Tt.Token (
  Token(Date, Time, Colon, Number, Priority, Sym, Comment, Text),
  showToken,
  tokenParser,
) where

import           Control.Monad
import           Data.Char
import           Data.Fixed         (Pico)
import           Data.Time
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Text.Printf
import           Tt.Util

-- | Parts of a todo.txt line item
data Token =
    Date Day                    -- ^ A calendar date, YYYY-mm-dd

  | Time TimeOfDay TimeZone     -- ^ A time of day, HH:MM:SS-HHMM
                                --
                                --   (we really want ZonedTime here but it
                                --   doesn't implement Eq which we want to
                                --   derive for the Token type)

  | Colon Token Token           -- ^ Name value pair
  | Number Rational             -- ^ A numerical value, 1, -8 or 3.1415
  | Priority Char               -- ^ Priority tag, "(A)" etc.
  | Sym String                  -- ^ A symbolic identifier
  | Comment String              -- ^ Comment text after a double hyphen
  | Text String                 -- ^ Whitespace separated anything else
    deriving (Eq, Show)

-- | Pretty-print a Token
showToken :: Token -> String
showToken (Date d) = show d
-- Don't show fractional seconds.
showToken (Time t z) =
  formatTime defaultTimeLocale "%H:%M:%S%z" (toZonedTime t z)
showToken (Colon t u  ) = showToken t ++ ":" ++ showToken u
showToken (Number   n ) = showRat n
showToken (Priority ch) = printf "(%c)" ch
showToken (Comment  c ) = printf "--%s" c
showToken (Sym      t ) = t
showToken (Text     t ) = t

toZonedTime :: TimeOfDay -> TimeZone -> ZonedTime
toZonedTime t = ZonedTime (LocalTime (ModifiedJulianDay 0) t)


-- | Parse a string into a Token
tokenParser :: Parser Token
tokenParser =
  wrap date
    <|> wrap zonedTime
    <|> wrap colon
    <|> wrap number
    <|> wrap priority
    <|> wrap sym
    <|> wrap comment
    <|> wrap text
 where
  wrap :: Parser Token -> Parser Token
  wrap parser = try (parser <* (eof <|> void (many1 space)))

date :: Parser Token
date =
  Date <$> (fromGregorian <$> year <* char '-' <*> month <* char '-' <*> day)

zonedTime :: Parser Token
zonedTime = Time <$> timeOfDay <*> zoneOffset

colon :: Parser Token
colon = Colon <$> sym <* char ':' <*> (sym <|> date <|> zonedTime <|> number)

number :: Parser Token
number = Number <$> plusMinus

priority :: Parser Token
priority = Priority <$> (char '(' *> (letter <|> digit) <* char ')')

sym :: Parser Token
sym = Sym <$> symbol

comment :: Parser Token
comment =
  Comment <$> (string "--" *> manyTill anyChar (void (char '\n') <|> eof))

text :: Parser Token
text = Text <$> many1 (satisfy (not . isSpace))

-- XXX: Currently numbers that start with + can parse as digits, this is
-- hacked around in tokenParser by trying to parse the input as a number
-- before trying to parse it as a symbol.

symbol :: Parser String
symbol = (:) <$> firstChar <*> many restChars
 where
  firstChar = letter <|> oneOf "_+@"
  restChars = letter <|> digit <|> oneOf "_+@-"

timeOfDay :: Parser TimeOfDay
timeOfDay = TimeOfDay <$> hour <* char ':' <*> minute <* char ':' <*> second

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

plusMinus :: Parser Rational
plusMinus =
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
