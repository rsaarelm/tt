module Tt.Db (
  Db,
  DbConf,
  dbConf,
  readDb,
  dbAppend,
) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.FilePath  (joinPath)
import           Tt.Entry

type Db = [Entry]

-- | List of paths used for the database, active todo list should be last.
type DbConf = [FilePath]

readDb :: DbConf -> IO Db
readDb conf = sortOn entrySortKey . join <$> traverse parseFile conf

dbConf :: IO DbConf
dbConf = traverse homeFilePath ["done.txt", "todo.txt"]
 where
  homeFilePath filename = do
    home <- getHomeDirectory
    return $ joinPath [home, filename]

parseFile :: FilePath -> IO Db
parseFile path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      return $ mapMaybe parseEntry $ lines contents
    else error ("File " ++ path ++ " not found")

dbAppend :: DbConf -> String -> IO ()
dbAppend conf entry = appendFile (last conf) (entry ++ "\n")
