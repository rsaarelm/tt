module Main where

import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Time
import           GHC.Exts
import           Numeric.Interval.NonEmpty
import           Options.Applicative
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.Exit
import           System.FilePath  (joinPath)
import           System.IO
import           Text.Printf
import           Tt.Entry
import           Tt.Goal
import qualified Tt.Msg                    as Msg
import           Tt.Parser
import           Tt.Util
import           Tt.Work
import           Options

main :: IO ()
main = do
  options <- execParser options
  -- This part is hacky. The prefix option has default value "~/", but we
  -- can't expand literal ~-paths, and when the user enters a ~-path on command
  -- line, the shell is expected to expand it to an absolute path. So the
  -- default value is special cased here to read the home directory.
  --
  -- The cleaner approach would be to just make the prefix option a Maybe
  -- String and have the branch for Nothing, but the string value is a bit
  -- more informative if it shows up in generated CLI options documentation.
  prefix <- case prefix options of
               "~/" -> getHomeDirectory
               x -> return x
  db <- sortOn entrySortKey . join <$> traverse (slurp prefix) ["done.txt", "todo.txt"]
  now <- getZonedTime
  case runCmd (Ctx now db) (cmd options) of
    Left error -> die error
    Right (CmdResult entries msg) ->
      do
        traverse_ (spew prefix "todo.txt") entries
        forM_ msg putStrLn
 where
  -- Read file into entries
  slurp prefix file = do
   let path = joinPath [prefix, file]
   fileExists <- doesFileExist path
   if fileExists then mapMaybe parseEntry . lines <$> readFile path
                 else return []
  -- Write a new line to a file
  spew prefix file line =
    do
      printf "Wrote to todo.txt: %s\n" line
      appendFile (joinPath [prefix, file]) (line ++ "\n")

type Db = [RawEntry]

data Ctx = Ctx { now :: ZonedTime, db :: Db }

-- Result of a successful command execution.
--
-- Can catenate lines to todo.txt and print a message to stdout.
data CmdResult = CmdResult { catenate :: [String], cmdMessage :: Maybe String }

runCmd :: Ctx -> Cmd -> Either String CmdResult
runCmd _ _ = Left "TODO"

clockIn :: String -> [String] -> IO ()
clockIn project text = do
  current <- getCurrentProject
  when (isJust current) (clockOut [])
  now <- getZonedTime
  append $ Msg.clockIn now project (unwords text)
  printf "Clocked into %s.\n" project

clockOut :: [String] -> IO ()
clockOut text = do
  current <- getCurrentProject
  case current of
    Just project -> do
      now <- getZonedTime
      append $ Msg.clockOut now (unwords text)
      printf "Clocked out of %s.\n" project
    Nothing -> printf "Error: Not clocked in a project.\n"

getCurrentProject :: IO (Maybe String)
getCurrentProject = currentProject <$> loadWork

todo :: [String] -> IO ()
todo text = do
  now <- getZonedTime
  let msg = Msg.todo now (unwords text)
  append msg
  printf "Todo task added: %s\n" msg

done :: [String] -> IO ()
done text = do
  now <- getZonedTime
  let msg          = processMsg now (unwords text)
  append msg
  printf "Done task added: %s\n" msg
 where
  -- If the message starts with "^", backdate it to yesterday
  processMsg t c@('^':cs) = puntBack t c
  processMsg t c          = Msg.done t c
  puntBack t ('^':cs) = puntBack (yesterday t) cs
  puntBack t s        = Msg.done t s

timeclock :: IO ()
timeclock = do
  work <- loadUnsealedWork
  mapM_ putStrLn (timeClocks work)


current :: IO ()
current = do
  work  <- loadWork
  today <- today
  now   <- getZonedTime
  case currentProject work of
    Just project -> do
      let todaysTime = duration $ onCurrentProject work `during` today
      printf "%s %s\n" project (showHours todaysTime)
    Nothing -> case plannedProject work (zonedTimeToLocalTime now) of
      Just (p, s) -> printf "%s until %s\n" p endTime
       where
        endTime = formatTime defaultTimeLocale "%H:%M" $ sup (asTimeInterval s)
      Nothing -> return ()


goals :: IO ()
goals = do
  goals <- loadGoals
  now   <- getZonedTime
  unless (null goals) $ showGoals goals now
 where
  showGoals goals now = do
    printf
      "goal               current (target)   deadline                 failures\n"
    printf
      "------------------|------------------|------------------------|--------\n"
    mapM_ (printGoal now) goals

printGoal :: ZonedTime -> (Project, Goal) -> IO ()
printGoal now (p, g) = printf
  "%-18s %-18s %-24s %s\n"
  p
  ( printf "%s %s %s"
           (showUnit (goalValue g) (goalUnit g))
           (if goalSlope g > 0 then "↑" else "↓")
           (showUnit (fromIntegral $ ceiling (goalTarget g')) (goalUnit g')) :: String
  )
  (Msg.deadline now (failureTime g))
  (if failureCount g > 0 then show (failureCount g) else "")
 where
  -- Show target point at next midnight where today's goal failure will be
  -- checked.
  g' = updateGoalClock g nextMidnight
  nextMidnight =
    LocalTime (1 `addDays` localDay (zonedTimeToLocalTime now)) midnight


loadDb :: IO Db
loadDb = do
  conf <- dbConf
  readDb conf

append :: String -> IO ()
append entry = do
  conf <- dbConf
  dbAppend conf entry

loadWork :: IO WorkState
loadWork = do
  work <- loadUnsealedWork
  now  <- getZonedTime
  return $ seal now work

-- | Unsealed work will not have a session for the currently open project, but
-- it's what you want if you're printing timeclocks.
loadUnsealedWork :: IO WorkState
loadUnsealedWork = toWorkState <$> loadDb

loadGoals :: IO [(Project, Goal)]
loadGoals = do
  work <- loadWork
  now  <- getZonedTime
  return $ sortWith (\(_, g) -> failureTime g) $ map
    (\(p, g) -> (p, updateGoalClock g (zonedTimeToLocalTime now)))
    (activeGoals (entries work))
