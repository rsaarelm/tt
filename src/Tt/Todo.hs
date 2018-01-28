module Tt.Todo (
  todoEntry,
  doneEntry,
) where

import Data.Time
import Tt.Token
import Tt.Db

todoEntry :: ZonedTime -> String -> Entry
todoEntry t message = timeStamp (dayOf t) $ tokenize message

doneEntry :: ZonedTime -> String -> Entry
doneEntry t message = markDone (dayOf t) $ tokenize message

timeStamp :: Day -> Entry -> Entry
-- Priority above date for todo
timeStamp t (Priority c:es) = Priority c:timeStamp t es
timeStamp t es = Date t:es

markDone :: Day -> Entry -> Entry
-- Drop priority for done
markDone t (Priority _:es) = markDone t es
markDone t es = Sym "x":Date t:es

dayOf :: ZonedTime -> Day
dayOf t = localDay (zonedTimeToLocalTime t)
