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
) where

import           Data.Time
import           Numeric.Interval.NonEmpty
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

instance Num a => Semigroup (Value a) where
  (Add a) <> (Add b) = Add (a + b)
  -- Calculate initial baseline from pre-existing addition
  (Add a) <> (Set b c) = Set (b - a) c
  -- Update value but maintain original baseline
  (Set a b) <> (Add c) = Set a (b + c)
  (Set a _) <> (Set _ b) = Set a b

instance Num a => Monoid (Value a) where
  mempty = Add 0

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
