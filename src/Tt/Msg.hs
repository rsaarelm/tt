module Tt.Msg (
  clockIn,
  clockOut,
  timeclockIn,
  timeclockOut,
  todo,
  done
) where

import           Data.Time
import           Text.Printf

-- | Clock in message in todo.txt format
--
-- These messages use 's', 'e' instead of 'i', 'o' so that lexically sorting a
-- todo.txt file will put the clock in line after the clock out line if both
-- have the exact same timestamp.
clockIn :: ZonedTime -> String -> String -> String
clockIn t proj "" = printf "x %s %s s %s" (date t) (zonedTimeOfDay t) proj
clockIn t proj msg =
  printf "x %s %s s %s %s" (date t) (zonedTimeOfDay t) proj msg

-- | Clock out message in todo.txt format
clockOut :: ZonedTime -> String -> String
clockOut t ""  = printf "x %s %s e" (date t) (zonedTimeOfDay t)
clockOut t msg = printf "x %s %s e %s" (date t) (zonedTimeOfDay t) msg

-- | Clock in message in the emacs/hledger timeclock format
--
-- The signature is simpler than in clockIn/clockOut because these are
-- generated from Entries that drop time zone data and comments.
timeclockIn :: LocalTime -> String -> String
timeclockIn t = printf "i %s %s" (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" t)

-- | Clock out message in the emacs/hledger timeclock format
timeclockOut :: LocalTime -> String
timeclockOut t = printf "o %s" (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" t)

todo :: ZonedTime -> String -> String
-- Put priority tag in front of the timestamp
todo t ('(':priority:')':' ':msg) = printf "(%c) %s %s" priority (date t) msg
todo t msg                        = printf "%s %s" (date t) msg

done :: ZonedTime -> String -> String
done t = printf "x %s %s" (date t)

date :: ZonedTime -> String
date = formatTime defaultTimeLocale "%Y-%m-%d"

zonedTimeOfDay :: ZonedTime -> String
zonedTimeOfDay = formatTime defaultTimeLocale "%H:%M:%S%z"
