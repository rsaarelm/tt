module Tt.Work (
  WorkState(currentProject),
  WorkUnit(Quantity, Span),
  parseWork,
  workInterval,
  onProject,
  onCurrentProject,
  duration,
  during
) where

import           Control.Arrow             (second)
import           Data.Maybe
import           Data.Time                 hiding (parseTime)
import           Numeric.Interval.NonEmpty hiding (inflate)
import           Tt.Clock
import           Tt.Db
import           Tt.Token
import           Tt.Util

type Project = String
type Unit = Maybe String

data WorkState = WorkState {
  currentProject :: Maybe Project,
  projectUnits   :: [(Project, WorkUnit)]
}

-- | An unit of work done on a project
--
-- Work can be measured as a span of time or with some other quantity and
-- occurring at a specific timepoint.
data WorkUnit =
    Quantity Day Rational Unit
  | Span (Interval UTCOrd)

-- | Parse work entries from data.
--
-- Needs current time in case there's an open clock entry that extends to the
-- present
parseWork :: ZonedTime -> Db -> WorkState
parseWork now db = WorkState (openProject rawClocks)
                             (singleEntries ++ clockSessions)
 where
  singleEntries = mapMaybe parseEntry db
  clockSessions = map (second Span) $ clockWork $ closeClocks now rawClocks
  rawClocks     = clocks db

workInterval :: WorkUnit -> Maybe (Interval UTCOrd)
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
  parseTime (Sym "x":Date day:Time t z:es) =
    Just (day, UTCOrd (ZonedTime (LocalTime day t) z), es)
  parseTime (Sym "x":Date day:es) =
    -- XXX: Span datapoints without a zoned time of day will default to UTC
    -- midday, this will cause them to fall between days if you're a +1200
    -- time zone.
    Just (day, UTCOrd (ZonedTime (LocalTime day midday) utc), es)
  parseTime _ = Nothing

  parseProject (Sym project:es) = Just (project, es)
  parseProject _                = Nothing

  parseValue _ t (Number num:Sym "h":_) = Span
    (inflate (fromIntegral (truncate (num * 1800) :: Integer)) (singleton t))
  parseValue _ t (Number num:Sym "min":_) =
    Span (inflate (fromIntegral (truncate (num * 30) :: Integer)) (singleton t))
  parseValue day _ (Number num:Sym unit:_) = Quantity day num (Just unit)
  parseValue day _ (Number num         :_) = Quantity day num Nothing
  parseValue day _ _                       = Quantity day 1 Nothing

inflate :: NominalDiffTime -> Interval UTCOrd -> Interval UTCOrd
inflate dt i =
  mapUTCOrd (addUTCTime (-dt)) (inf i) ... mapUTCOrd (addUTCTime dt) (sup i)

duration :: [WorkUnit] -> NominalDiffTime
duration units = sum $ map unitDuration units
 where
  unitDuration (Span s) = u (sup s) `diffUTCTime` u (inf s)
  unitDuration _        = 0
  u = zonedTimeToUTC . fromUTCOrd


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
intersectWork s (Span workSpan) = Span <$> workSpan'
 where
  workSpan' = intersection
    workSpan
    (zonify (inf workSpan) (inf s) ... zonify (sup workSpan) (sup s))
  zonify (UTCOrd zt) lt = UTCOrd $ ZonedTime lt (zonedTimeZone zt)
