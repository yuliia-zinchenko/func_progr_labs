-- | Human-readable messages for exceptions raised while working with the database.
-- Shared by the console and the web interface.
module Errors (errorMessage, isFatal) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException)
import Data.Maybe (isJust)
import qualified Database.MySQL.Base as Base
import Database.MySQL.Simple (FormatError (..), QueryError (..))
import Database.MySQL.Simple.Result (ResultError)
import Input (UserAbort (..))
import System.Exit (ExitCode)

errorMessage :: SomeException -> String
errorMessage e
  | Just (UserAbort msg) <- fromException e = msg
  | Just dbErr <- fromException e = describeDbError dbErr
  | Just qe <- fromException e = "Query error: " ++ qeMessage qe
  | Just fe <- fromException e = "Query format error: " ++ fmtMessage fe
  | Just (re :: ResultError) <- fromException e = "Result conversion error: " ++ show re
  | otherwise = "Error: " ++ displayException e

-- | Exceptions that must not be swallowed: program exit and Ctrl+C.
isFatal :: SomeException -> Bool
isFatal e =
  isJust (fromException e :: Maybe ExitCode) || isJust (fromException e :: Maybe SomeAsyncException)

describeDbError :: Base.MySQLError -> String
describeDbError e = case Base.errNumber e of
  1062 -> "Duplicate value: " ++ Base.errMessage e
  1451 -> "Cannot delete or change: the record is used by other records (delete them first)."
  1452 -> "Referenced record does not exist."
  3819 -> "Check constraint failed: " ++ Base.errMessage e
  n -> "Database error " ++ show n ++ ": " ++ Base.errMessage e
