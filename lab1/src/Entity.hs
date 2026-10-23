{-# LANGUAGE ScopedTypeVariables #-}

-- | The core of the lab: type classes and generic DB operations.
-- Every DB table is a separate data type that becomes an instance of
-- 'Entity', after which all CRUD operations work for it automatically.
module Entity
  ( Entity(..)
  , Pretty(..)
  , selectAll
  , selectWhere
  , selectById
  , insertEntity
  , updateEntity
  , deleteEntity
  ) where

import           Data.Int (Int32)
import           Data.List (intercalate)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy (Proxy (..))
import           Data.String (fromString)
import           Database.MySQL.Base
import qualified System.IO.Streams as Streams

-- | A DB entity: everything we need to know about a type
-- to store it in a MySQL table.
class Entity a where
  tableName :: Proxy a -> String          -- ^ table name
  columns   :: Proxy a -> [String]        -- ^ columns (without id)
  toRow     :: a -> [MySQLValue]          -- ^ values for INSERT/UPDATE
  fromRow   :: [MySQLValue] -> Maybe a    -- ^ parse a SELECT row
  entityId  :: a -> Int32                 -- ^ primary key

-- | Human-readable console output.
class Pretty a where
  pretty :: a -> String

selectList :: Entity a => Proxy a -> String
selectList p =
  "SELECT id, " ++ intercalate ", " (columns p) ++ " FROM " ++ tableName p

-- | All rows of the table.
selectAll :: forall a. Entity a => MySQLConn -> IO [a]
selectAll conn = do
  let p = Proxy :: Proxy a
  (_, is) <- query_ conn (fromString (selectList p ++ " ORDER BY id"))
  mapMaybe fromRow <$> Streams.toList is

-- | Rows matching an arbitrary WHERE condition with parameters.
selectWhere :: forall a. Entity a => MySQLConn -> String -> [MySQLValue] -> IO [a]
selectWhere conn cond params = do
  let p = Proxy :: Proxy a
  (_, is) <- query conn (fromString (selectList p ++ " WHERE " ++ cond)) params
  mapMaybe fromRow <$> Streams.toList is

-- | A single row by id.
selectById :: Entity a => MySQLConn -> Int32 -> IO (Maybe a)
selectById conn i = listToMaybe <$> selectWhere conn "id = ?" [MySQLInt32 i]

-- | Insert; returns the id of the new row.
insertEntity :: forall a. Entity a => MySQLConn -> a -> IO Int32
insertEntity conn x = do
  let p    = Proxy :: Proxy a
      cols = columns p
      q    = "INSERT INTO " ++ tableName p
          ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
          ++ intercalate ", " (replicate (length cols) "?") ++ ")"
  ok <- execute conn (fromString q) (toRow x)
  pure (fromIntegral (okLastInsertID ok))

-- | Update a row by its id.
updateEntity :: forall a. Entity a => MySQLConn -> a -> IO ()
updateEntity conn x = do
  let p = Proxy :: Proxy a
      q = "UPDATE " ++ tableName p
       ++ " SET " ++ intercalate ", " [c ++ " = ?" | c <- columns p]
       ++ " WHERE id = ?"
  _ <- execute conn (fromString q) (toRow x ++ [MySQLInt32 (entityId x)])
  pure ()

-- | Delete a row by id.
deleteEntity :: Entity a => Proxy a -> MySQLConn -> Int32 -> IO ()
deleteEntity p conn i = do
  _ <- execute conn
         (fromString ("DELETE FROM " ++ tableName p ++ " WHERE id = ?"))
         [MySQLInt32 i]
  pure ()
