{-# LANGUAGE OverloadedStrings #-}

-- | Type classes of the information system. Every table type gets instances of them
-- (see "Instances.*"), and the generic menu in "Menu" works through these classes only.
module Classes
  ( DbEnum (..)
  , Entity (..)
  , Repository (..)
  , Referable (..)
  , Displayable (..)
  , Inputable (..)
  , Validatable (..)
  , ensure
  , checkRecord
  , issues
  , countRows
  ) where

import Control.Monad (void)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults)

-- | Enumeration stored in a MySQL ENUM column.
class (Eq a, Enum a, Bounded a) => DbEnum a where
  -- | Value as written in the database.
  toDb :: a -> String

  allValues :: [a]
  allValues = [minBound .. maxBound]

  fromDb :: String -> Maybe a
  fromDb s = lookup s [(toDb x, x) | x <- allValues]

-- | Record mapped to a database table. 'QueryParams' renders all columns except `id`,
-- 'QueryResults' reads `id` followed by 'columns'.
class (QueryResults a, QueryParams a) => Entity a where
  entityName :: Proxy a -> String
  tableName :: Proxy a -> String
  columns :: Proxy a -> [String]
  entityId :: a -> Int

-- | CRUD operations. Default implementations build SQL from the 'Entity' description,
-- so an instance only has to override what is specific for its table.
class Entity a => Repository a where
  findAll :: Connection -> IO [a]
  findAll conn = query_ conn (fromString (selectSql (Proxy :: Proxy a) ++ " ORDER BY id"))

  findById :: Connection -> Int -> IO (Maybe a)
  findById conn i =
    listToMaybe <$> query conn (fromString (selectSql (Proxy :: Proxy a) ++ " WHERE id = ?")) (Only i)

  -- | Insert a new row and return its generated id.
  insert :: Connection -> a -> IO Int
  insert conn x = do
    let p = Proxy :: Proxy a
        cols = columns p
        sql =
          "INSERT INTO " ++ tableName p ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
            ++ intercalate ", " (map (const "?") cols) ++ ")"
    void (execute conn (fromString sql) x)
    fromIntegral <$> insertID conn

  update :: Connection -> a -> IO ()
  update conn x = do
    let p = Proxy :: Proxy a
        sql =
          "UPDATE " ++ tableName p ++ " SET "
            ++ intercalate ", " [c ++ " = ?" | c <- columns p] ++ " WHERE id = ?"
    void (execute conn (fromString sql) (renderParams x ++ [render (entityId x)]))

  -- | Delete by id; returns False when nothing was deleted.
  remove :: Proxy a -> Connection -> Int -> IO Bool
  remove p conn i = do
    n <- execute conn (fromString ("DELETE FROM " ++ tableName p ++ " WHERE id = ?")) (Only i)
    pure (n > 0)

  -- | Business rules that need the database (overlaps in the schedule etc.).
  -- Returns a list of problems; empty list means the record can be saved.
  conflicts :: Connection -> a -> IO [String]
  conflicts _ _ = pure []

selectSql :: Entity a => Proxy a -> String
selectSql p = "SELECT " ++ intercalate ", " ("id" : columns p) ++ " FROM " ++ tableName p

-- | Entity that can be chosen by id from another table (foreign key).
class Repository a => Referable a where
  refLabel :: a -> String

  -- | (id, short description) of all rows, shown when the user has to pick one.
  refOptions :: Connection -> Proxy a -> IO [(Int, String)]
  refOptions conn _ = map (\x -> (entityId x, refLabel x)) <$> (findAll conn :: IO [a])

-- | Presentation of records as table rows.
class Displayable a where
  headers :: Proxy a -> [String]
  cells :: a -> [String]

-- | Interactive input of a new record and correction of an existing one.
class Inputable a where
  readNew :: Connection -> IO a
  -- | Ask every field again; empty input keeps the old value.
  editFields :: Connection -> a -> IO a

-- | Checks that do not need the database.
class Validatable a where
  validate :: a -> Either String a
  validate = Right

-- | All problems that prevent saving a record: validation first, then database rules.
checkRecord :: (Repository a, Validatable a) => Connection -> a -> IO [String]
checkRecord conn x = case validate x of
  Left err -> pure [err]
  Right valid -> conflicts conn valid

-- | Fail with a message when the condition does not hold.
ensure :: Bool -> String -> Either String ()
ensure ok msg = if ok then Right () else Left msg

-- | Messages of the checks that failed.
issues :: [(Bool, String)] -> [String]
issues checks = [msg | (failed, msg) <- checks, failed]

-- | Run a `SELECT COUNT(*) ...` query.
countRows :: QueryParams q => Connection -> Query -> q -> IO Int
countRows conn q ps = do
  rows <- query conn q ps
  pure $ case rows of
    [Only n] -> n
    _ -> 0
