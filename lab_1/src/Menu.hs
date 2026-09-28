{-# LANGUAGE OverloadedStrings #-}

-- | Console menus. Data management is one generic CRUD menu that works for any type
-- with the required class instances.
module Menu (mainMenu) where

import Classes
import Control.Exception (Handler (..), SomeException, catches, displayException, throwIO)
import Data.Proxy (Proxy (..))
import qualified Database.MySQL.Base as Base
import Database.MySQL.Simple (Connection, FormatError (..), QueryError (..))
import Database.MySQL.Simple.Result (ResultError)
import Input
import Instances.Classroom ()
import Instances.Discipline ()
import Instances.FreeAccess ()
import Instances.Lesson ()
import Instances.Maintenance ()
import Instances.Session ()
import Instances.Teacher ()
import Instances.User ()
import Instances.Workstation ()
import Reports (reports)
import System.Exit (ExitCode)
import Table
import Types

-- | Table type packed together with its instances.
data TableSpec = forall a. (Repository a, Displayable a, Inputable a, Validatable a) => TableSpec String (Proxy a)

tables :: [TableSpec]
tables =
  [ TableSpec "Classrooms" (Proxy :: Proxy Classroom)
  , TableSpec "Workstations" (Proxy :: Proxy Workstation)
  , TableSpec "Users" (Proxy :: Proxy User)
  , TableSpec "Teachers" (Proxy :: Proxy Teacher)
  , TableSpec "Disciplines" (Proxy :: Proxy Discipline)
  , TableSpec "Schedule (planned lessons)" (Proxy :: Proxy Lesson)
  , TableSpec "Free access time" (Proxy :: Proxy FreeAccess)
  , TableSpec "Workstation sessions (usage)" (Proxy :: Proxy Session)
  , TableSpec "Maintenance log" (Proxy :: Proxy Maintenance)
  ]

mainMenu :: Connection -> IO ()
mainMenu conn = do
  choice <- chooseMenu "Faculty display classes schedule" "Exit" ["Manage data", "Reports and queries"]
  case choice of
    0 -> putStrLn "Bye!"
    1 -> tablesMenu conn >> mainMenu conn
    _ -> reportsMenu conn >> mainMenu conn

tablesMenu :: Connection -> IO ()
tablesMenu conn = do
  choice <- chooseMenu "Manage data" "Back" [title | TableSpec title _ <- tables]
  if choice == 0
    then pure ()
    else case tables !! (choice - 1) of
      TableSpec title p -> crudMenu conn title p >> tablesMenu conn

reportsMenu :: Connection -> IO ()
reportsMenu conn = do
  choice <- chooseMenu "Reports and queries" "Back" (map fst reports)
  if choice == 0
    then pure ()
    else do
      safely (snd (reports !! (choice - 1)) conn)
      reportsMenu conn

-- | List / find / add / edit / delete for any table type.
crudMenu :: forall a. (Repository a, Displayable a, Inputable a, Validatable a) => Connection -> String -> Proxy a -> IO ()
crudMenu conn title p = do
  choice <- chooseMenu title "Back" ["List all", "Find by ID", "Add", "Edit", "Delete"]
  case choice of
    0 -> pure ()
    1 -> again (findAll conn >>= printRecords p)
    2 -> again (withRecord (\x -> printRecords p [x]))
    3 -> again ((readNew conn :: IO a) >>= \x -> save x (insert conn x >>= \i -> putStrLn ("Saved, new id = " ++ show i)))
    4 -> again (withRecord edit)
    _ -> again (withRecord del)
  where
    again action = safely action >> crudMenu conn title p

    withRecord :: (a -> IO ()) -> IO ()
    withRecord action = do
      i <- ask "ID"
      found <- findById conn i
      maybe (putStrLn ("No " ++ entityName p ++ " with id " ++ show i ++ ".")) action found

    edit x = do
      printRecords p [x]
      putStrLn "Enter new values (empty input keeps the current value):"
      x' <- editFields conn x
      save x' (update conn x' >> putStrLn "Saved.")

    del x = do
      printRecords p [x]
      ok <- confirm ("Delete this " ++ entityName p ++ "?")
      if ok
        then do
          deleted <- remove p conn (entityId x)
          putStrLn (if deleted then "Deleted." else "Nothing deleted.")
        else putStrLn "Cancelled."

    -- validation without DB, then business rules with DB, then the write itself
    save :: a -> IO () -> IO ()
    save x write = case validate x of
      Left err -> putStrLn ("Not saved: " ++ err)
      Right valid -> do
        problems <- conflicts conn valid
        if null problems
          then write
          else putStrLn "Not saved:" >> mapM_ (putStrLn . ("  - " ++)) problems

-- | Run an action and report errors instead of crashing the program.
safely :: IO () -> IO ()
safely action =
  action
    `catches` [ Handler (\(e :: ExitCode) -> throwIO e)
              , Handler (\(UserAbort msg) -> putStrLn msg)
              , Handler (\(e :: Base.MySQLError) -> putStrLn (describeDbError e))
              , Handler (\(e :: QueryError) -> putStrLn ("Query error: " ++ qeMessage e))
              , Handler (\(e :: FormatError) -> putStrLn ("Query format error: " ++ fmtMessage e))
              , Handler (\(e :: ResultError) -> putStrLn ("Result conversion error: " ++ show e))
              , Handler (\(e :: SomeException) -> putStrLn ("Error: " ++ displayException e))
              ]

describeDbError :: Base.MySQLError -> String
describeDbError e = case Base.errNumber e of
  1062 -> "Duplicate value: " ++ Base.errMessage e
  1451 -> "Cannot delete or change: the record is used by other records (delete them first)."
  1452 -> "Referenced record does not exist."
  3819 -> "Check constraint failed: " ++ Base.errMessage e
  n -> "Database error " ++ show n ++ ": " ++ Base.errMessage e
