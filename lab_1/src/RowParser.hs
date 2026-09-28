-- | Small applicative parser that turns a MySQL result row into a Haskell record.
--
-- Used to write 'QueryResults' instances as
-- @convertResults = parseRow (Classroom \<$\> field \<*\> field \<*\> ...)@.
module RowParser (RowParser, field, parseRow) where

import Data.ByteString (ByteString)
import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple.QueryResults (convertError)
import Database.MySQL.Simple.Result (Result (..))

type Column = (Field, Maybe ByteString)

-- | Number of columns consumed and the parsing function.
data RowParser a = RowParser Int ([Column] -> Maybe (a, [Column]))

instance Functor RowParser where
  fmap f (RowParser n p) = RowParser n $ \cols -> do
    (a, rest) <- p cols
    pure (f a, rest)

instance Applicative RowParser where
  pure x = RowParser 0 $ \cols -> Just (x, cols)
  RowParser n pf <*> RowParser m pa = RowParser (n + m) $ \cols -> do
    (f, rest) <- pf cols
    (a, rest') <- pa rest
    pure (f a, rest')

-- | Read the next column.
field :: Result a => RowParser a
field = RowParser 1 next
  where
    next ((f, v) : rest) = Just (convert f v, rest)
    next [] = Nothing

-- | Run the parser on a whole row; the row must have exactly the expected number of columns.
parseRow :: RowParser a -> [Field] -> [Maybe ByteString] -> a
parseRow (RowParser n p) fs vs = case p (zip fs vs) of
  Just (a, []) -> a
  _ -> convertError fs vs n
