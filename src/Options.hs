module Options
  ( Options(prefix, cmd)
  , Cmd
    ( In
    , Out
    , Break
    , Todo
    , Done
    , Goals
    , Current
    , NextPing
    , MissedPings
    , LogPing
    , Timeclock
    )
  , options
  )
where

import           Data.Maybe
import           Data.Semigroup                           ( (<>) )
import           Data.Int
import           Options.Applicative

data Options = Options
  { prefix :: FilePath
  , cmd :: Cmd
  } deriving Show

data Cmd
  = In
    { at :: Maybe String
    , project :: String
    , comment :: Maybe String
    }
  | Out
    { at :: Maybe String
    , comment :: Maybe String
    }
  | Break
    { for :: String
    , comment :: Maybe String
    }
  | Todo
    { msg :: String
    }
  | Done
    { timestamp :: Bool
    , msg :: String
    }
  | Goals
  | Current
  | NextPing { intervalMinutes :: Int, now :: Maybe Int64 }
  | MissedPings { approxCount :: Int, project :: String, intervalMinutes :: Int }
  | LogPing { intervalMinutes :: Int, project :: String, comment :: Maybe String }
  | Timeclock
  deriving Show

options :: ParserInfo Options
options = info (p <**> helper) (fullDesc <> header "tt - Time tracking tool")
  where p = Options <$> parseSettings <*> parseCmd

parseSettings = strOption
  (long "prefix" <> metavar "PATH" <> value "~/" <> help
    "Path where to find todo.txt and done.txt"
  )

parseCmd :: Parser Cmd
parseCmd =
  subparser
    $  command
         "in"
         ( info
             (   In
             <$> (atOpt <|> inOpt)
             <*> argument str (metavar "project")
             <*> (maybeWords <$> many (argument str (metavar "comment")))
             )
         $ progDesc "Clock in to a project"
         )
    <> command
         "out"
         ( info
             (   Out
             <$> (atOpt <|> inOpt <|> afterOpt)
             <*> (maybeWords <$> many (argument str (metavar "comment")))
             )
         $ progDesc "Clock out of the current clock"
         )
    <> command
         "break"
         ( info
             (   Break
             <$> (forOpt <|> fromOpt)
             <*> (maybeWords <$> many (argument str (metavar "comment")))
             )
         $ progDesc "Add a break of specified duration to current clock"
         )
    <> command
         "current"
         ( info (pure Current)
         $ progDesc "Show today's hours and project name of current clock"
         )
    <> command
         "timeclock"
         ( info (pure Timeclock)
         $ progDesc "Output hours in timeclock format for hledger"
         )

    <> command
         "todo"
         ( info (Todo <$> (unwords <$> many (argument str (metavar "message"))))
         $ progDesc "Add a todo item from the command line"
         )
    <> command
         "done"
         ( info
             (   Done
             <$> switch
                   (long "timestamp" <> short 't' <> help "Include timestamp")
             <*> (unwords <$> many (argument str (metavar "message")))
             )
         $ progDesc "Add a done item from the command line"
         )

    <> command
         "next-ping"
         ( info
             (   NextPing
             <$> argument auto (metavar "interval-minutes")
             <*> optional (argument auto (metavar "posix-time-now"))
             )
         $ progDesc
             "Give time in seconds to sleep until next stochastic time tracking ping"
         )
    <> command
         "missed-pings"
         ( info
             (   MissedPings
             <$> option
                   auto
                   (short 'n' <> metavar "approx-count" <> value 20 <> help
                     "Approximate ping count"
                   )
             <*> option
                   str
                   (short 'p'
                    <> long "project"
                    <> metavar "project-name"
                    <> value "_"
                    <> help "Default project name for the missed pings"
                   )
             <*> argument auto (metavar "interval-minutes")
             )
         $ progDesc
             "Print blank log entries for recent missed pings"
         )
    <> command
         "log-ping"
         ( info
             (   LogPing
             <$> argument auto (metavar "interval-minutes")
             <*> argument str  (metavar "project")
             <*> (maybeWords <$> many (argument str (metavar "comment")))
             )
         $ progDesc "Log activity for latest ping directly"
         )

    <> command
         "goals"
         (info (pure Goals) $ progDesc "Show progress on currently active goals"
         )

 where
  atOpt = (option $ maybeStr <$> str)
    (long "at" <> metavar "TIME_EXPR" <> value Nothing <> help
      "Generic time adjustment"
    )

  inOpt = (option $ maybePrefix "in " <$> str)
    (long "in" <> metavar "RELTIME_EXPR" <> value Nothing <> help
      "Adjust to happen in [RELTIME_EXPR] time"
    )

  afterOpt = (option $ maybePrefix "after " <$> str)
    (long "after" <> metavar "RELTIME_EXPR" <> value Nothing <> help
      "Adjust to happen after total [TIME_EXPR] spent today"
    )

  forOpt = (option $ ("in " ++) <$> str)
    (long "for" <> metavar "RELTIME_EXPR" <> help "Duration for break")

  fromOpt = (option $ (++ " until now") <$> str)
    (long "from" <> metavar "TIME_EXPR" <> help
      "Adjust to have happened from [TIME_EXPR] until now"
    )

  maybeWords []       = Nothing
  maybeWords (x : xs) = Just $ unwords (x : xs)

  maybeStr [] = Nothing
  maybeStr x  = Just x

  maybePrefix p [] = Nothing
  maybePrefix p x  = Just (p ++ x)
