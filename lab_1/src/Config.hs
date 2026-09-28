-- | Database connection settings. Every value can be overridden by an environment variable.
module Config (loadConnectInfo) where

import Data.Maybe (fromMaybe)
import Database.MySQL.Base.Types (Option (..))
import Database.MySQL.Simple (ConnectInfo (..), defaultConnectInfo)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

loadConnectInfo :: IO ConnectInfo
loadConnectInfo = do
  host <- env "DB_HOST" "127.0.0.1"
  port <- env "DB_PORT" "3306"
  user <- env "DB_USER" "lab_user"
  pass <- env "DB_PASSWORD" "lab_pass"
  name <- env "DB_NAME" "display_classes"
  pure
    defaultConnectInfo
      { connectHost = host
      , connectPort = fromMaybe 3306 (readMaybe port)
      , connectUser = user
      , connectPassword = pass
      , connectDatabase = name
      , connectOptions = [CharsetName "utf8mb4"]
      }
  where
    env key def = fromMaybe def <$> lookupEnv key
