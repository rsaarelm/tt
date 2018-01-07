module Main where

import Control.Monad (join)
import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Tt

in_ :: String -> IO ()
in_ project = do
    putStrLn $ "in " ++ project

out :: IO ()
out = do
    putStrLn "out!"

opts :: Parser (IO ())
opts = subparser $
     command "in" (info (in_ <$> argument str idm) idm)
  <> command "out" (info (pure out) idm)

main :: IO ()
main = join $ execParser $ info opts $
    fullDesc
    <> progDesc "Time tracking tool"
    <> header "tt - time tracking tool"
