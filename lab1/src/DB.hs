-- | MySQL connection. Settings can be overridden with
-- the LAB1_DB_HOST / USER / PASS / NAME environment variables.
module DB (getConn) where

import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Database.MySQL.Base
import           System.Environment (lookupEnv)

getConn :: IO MySQLConn
getConn = do
  host <- fromMaybe "127.0.0.1"        <$> lookupEnv "LAB1_DB_HOST"
  user <- fromMaybe "lab1"             <$> lookupEnv "LAB1_DB_USER"
  pass <- fromMaybe "lab1pass"         <$> lookupEnv "LAB1_DB_PASS"
  name <- fromMaybe "faculty_services" <$> lookupEnv "LAB1_DB_NAME"
  -- defaultConnectInfoMB4 talks utf8mb4 to the server, matching the schema
  -- (the plain default is utf8mb3 and rejects 4-byte characters);
  -- encodeUtf8 keeps non-ASCII credentials intact.
  connect defaultConnectInfoMB4
    { ciHost     = host
    , ciUser     = encodeUtf8 (T.pack user)
    , ciPassword = encodeUtf8 (T.pack pass)
    , ciDatabase = encodeUtf8 (T.pack name)
    }
