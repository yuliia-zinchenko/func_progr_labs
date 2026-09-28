-- | Console menus. Data management is one generic CRUD menu that works for any type
-- with the required class instances.
module Menu (mainMenu) where

import Classes
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Time (Day, TimeOfDay)
import Database.MySQL.Simple (Connection)
import Errors (errorMessage, isFatal)
import Input
import Registry
import Reports
import Table
import Text.Read (readMaybe)

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
  choice <- chooseMenu "Reports and queries" "Back" (map reportTitle reports)
  if choice == 0
    then pure ()
    else do
      safely (runReport conn (reports !! (choice - 1)))
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

    save :: a -> IO () -> IO ()
    save x write = do
      problems <- checkRecord conn x
      if null problems
        then write
        else putStrLn "Not saved:" >> mapM_ (putStrLn . ("  - " ++)) problems

-- | Ask the report parameters in the console, run it and print the result tables.
runReport :: Connection -> Report -> IO ()
runReport conn r = do
  args <- Map.fromList <$> mapM askParam (reportParams r)
  reportRun r conn args >>= mapM_ printReportTable
  where
    askParam prm = (,) (rpName prm) <$> case rpKind prm of
      PRef target -> case lookupRef target of
        Just (RefSpec px) -> show <$> chooseRef conn px (rpLabel prm) Nothing
        Nothing -> throwIO (UserAbort ("Unknown table " ++ target))
      PDate -> askTyped (Proxy :: Proxy Day) prm
      PTime -> askTyped (Proxy :: Proxy TimeOfDay) prm
      PChoice options -> do
        opts <- options conn
        putStrLn (rpLabel prm ++ " - available: " ++ unwords opts)
        let loop = do
              v <- ask (rpLabel prm)
              if v `elem` opts then pure v else putStrLn "  Choose one of the listed values." >> loop
        loop

-- | Ask a typed value; the parameter default is offered and kept on empty input.
askTyped :: forall v. FieldInput v => Proxy v -> ReportParam -> IO String
askTyped _ prm =
  showField <$> case parseField (rpDefault prm) :: Maybe v of
    Just def -> askEdit (rpLabel prm) def
    Nothing -> ask (rpLabel prm)

printReportTable :: ReportTable -> IO ()
printReportTable t = do
  putStrLn ""
  if null (rtTitle t) then pure () else putStrLn (rtTitle t ++ ":")
  if null (rtRows t)
    then putStrLn (rtEmpty t)
    else printTable (rtHeaders t) (map drawBar (rtRows t))
  where
    drawBar row = case rtBar t of
      Just i -> [if j == i then bar c else c | (j, c) <- zip [0 ..] row]
      Nothing -> row
    bar s = case readMaybe s :: Maybe Double of
      Just pct -> let n = max 0 (min 20 (round (pct / 5))) in replicate n '#' ++ replicate (20 - n) '.'
      Nothing -> s

-- | Run an action and report errors instead of crashing the program.
safely :: IO () -> IO ()
safely action =
  action `catch` \(e :: SomeException) ->
    if isFatal e then throwIO e else putStrLn (errorMessage e)
