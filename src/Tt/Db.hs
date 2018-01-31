module Tt.Db (
  Entry,
  Db,
  DbConf,
  dbConf,
  readDb,
  showTokens,
  tokenize,
  dbAppend,
) where

import           Control.Monad
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.FilePath  (joinPath)
import           Text.Parsec
import           Tt.Token

type Entry = [Token]
type Db = [Entry]

-- | List of paths used for the database, active todo list should be last.
type DbConf = [FilePath]

readDb :: DbConf -> IO Db
readDb conf = join <$> traverse parseFile conf

dbConf :: IO DbConf
dbConf = traverse homeFilePath ["done.txt", "todo.txt"]
  where
    homeFilePath filename = do
        home <- getHomeDirectory
        return $ joinPath [home, filename]

parseFile :: FilePath -> IO Db
parseFile path = do
    exists <- doesFileExist path
    if exists then do
        contents <- readFile path
        return $ map tokenize $ lines contents
    else error ("File " ++ path ++ " not found")

showTokens :: Entry -> String
showTokens = unwords . map showToken

tokenize :: String -> Entry
tokenize text = (unwrap . parse tokenParser "") <$> words text
  where
    unwrap (Right x) = x
    unwrap (Left _)  = error ("Parsing '" ++ text ++ "' failed.")

dbAppend :: DbConf -> Entry -> IO ()
dbAppend conf entry = appendFile (last conf) (showTokens entry ++ "\n")
