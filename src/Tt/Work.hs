module Tt.Work (
  WorkState(currentProject),
  WorkUnit(Quantity, Span),
  parseWork,
  workInterval,
  onProject,
  onCurrentProject,
  duration,
  during,
  timeClocks
) where

import           Control.Arrow             (second)
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Time                 hiding (parseTime)
import           Numeric.Interval.NonEmpty
import           Tt.Clock
import           Tt.Db
import           Tt.Token
import           Tt.Util

type Project = String
type Unit = Maybe String

data WorkState = WorkState {
  currentProject :: Maybe Project,
  projectUnits   :: [(Project, WorkUnit)]
} deriving (Show)

-- | An unit of work done on a project
--
-- Work can be measured as a span of time or with some other quantity and
-- occurring at a specific timepoint.
data WorkUnit =
    Quantity Day Rational Unit
  | Span (Interval LocalTime)
    deriving (Show)

instance AsTimeInterval WorkUnit where
  asTimeInterval (Quantity day _ _) = singleton $ LocalTime day midday
  asTimeInterval (Span s          ) = s

-- | Parse work entries from data.
--
-- Needs current time in case there's an open clock entry that extends to the
-- present
parseWork :: ZonedTime -> Db -> WorkState
parseWork now db = WorkState (fmap fst openProject) $ sortBy
  (compare `on` inf . asTimeInterval . snd)
  (singleEntries ++ clockSessions ++ currentSession)
 where
  singleEntries            = mapMaybe parseEntry db
  clockSessions            = map (second Span) clockWork
  (clockWork, openProject) = clocks db
  currentSession           = maybe [] (\x -> [openToSpan x]) openProject
  openToSpan (name, start) = (name, Span (start ... zonedTimeToLocalTime now))

workInterval :: WorkUnit -> Maybe (Interval LocalTime)
workInterval (Span s) = Just s
workInterval _        = Nothing

-- | Filter work set to a specific project.
onProject :: WorkState -> String -> [WorkUnit]
onProject state name = map snd $ filter ((name ==) . fst) (projectUnits state)

onCurrentProject :: WorkState -> [WorkUnit]
onCurrentProject state = maybe [] (onProject state) (currentProject state)

-- | Parse an entry into a time or quantity describing atomic work item.
parseEntry :: Entry -> Maybe (Project, WorkUnit)
parseEntry entry = do
  (day, time, es) <- parseTime entry
  (project, es')  <- parseProject es
  return (project, parseValue day time es')
 where
  parseTime (Sym "x":Date day:Time t _:es) = Just (day, LocalTime day t, es)
  parseTime (Sym "x":Date day:es) = Just (day, LocalTime day midday, es)
  parseTime _ = Nothing

  parseProject (Sym project:es) = Just (project, es)
  parseProject _                = Nothing

  parseValue _ t (Number num:Sym "h":_) =
    Span
      $   t
      ... addLocalTime (fromIntegral (truncate (num * 3600) :: Integer)) t
  parseValue _ t (Number num:Sym "min":_) =
    Span $ t ... addLocalTime (fromIntegral (truncate (num * 60) :: Integer)) t
  parseValue day _ (Number num:Sym unit:_) = Quantity day num (Just unit)
  parseValue day _ (Number num         :_) = Quantity day num Nothing
  parseValue day _ _                       = Quantity day 1 Nothing

duration :: [WorkUnit] -> NominalDiffTime
duration units = sum $ map unitDuration units
 where
  unitDuration (Span s) = sup s `diffLocalTime` inf s
  unitDuration _        = 0


during :: [WorkUnit] -> Interval LocalTime -> [WorkUnit]
during units s = mapMaybe (intersectWork s) units

-- | Intersect a work unit with a local time interval
--
-- If the session is a time span, each end of the local time interval is
-- assigned the time zone of the corresponding span point.
intersectWork :: Interval LocalTime -> WorkUnit -> Maybe WorkUnit
intersectWork s x@(Quantity day _ _) | time `member` s = Just x
  where time = LocalTime day midday
intersectWork _ Quantity{}      = Nothing
intersectWork s (Span workSpan) = Span <$> s `intersection` workSpan

-- | Show a ClockEntry as a timeclock log line.
timeClocks :: WorkState -> [String]
timeClocks state = toClocks (projectUnits state)
 where
  -- XXX: Currently open task will be shown as closed at the time the command
  -- was run.
  toClocks ((name, Span s):ws) =
    [ unwords
        ["i", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" (inf s), name]
      , unwords ["o", formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" (sup s)]
      ]
      ++ toClocks ws
  toClocks (_:ws) = toClocks ws
  toClocks []     = []
