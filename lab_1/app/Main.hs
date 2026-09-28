module Main (main) where

import Config (loadConnectInfo)
import Control.Exception (bracket, try)
import qualified Database.MySQL.Base as Base
import Database.MySQL.Simple (ConnectInfo (..), close, connect)
import Menu (mainMenu)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdin, stdout, utf8)

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  info <- loadConnectInfo
  result <- try (connect info)
  case result of
    Left err -> do
      hPutStrLn stderr ("Cannot connect to MySQL at " ++ connectHost info ++ ": " ++ Base.errMessage err)
      hPutStrLn stderr "Check that the server is running and DB_* environment variables are correct."
      exitFailure
    Right conn -> bracket (pure conn) close mainMenu
