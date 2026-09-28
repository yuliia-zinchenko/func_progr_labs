{-# LANGUAGE OverloadedStrings #-}

-- | Console input: typed field parsing ('FieldInput') and prompts built on top of it.
module Input
  ( FieldInput (..)
  , UserAbort (..)
  , readLine
  , ask
  , askEdit
  , confirm
  , chooseMenu
  , chooseRef
  , chooseOptRef
  , parseEnum
  , enumHint
  ) where

import Classes
import Control.Exception (Exception, throwIO)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Time (Day, TimeOfDay, defaultTimeLocale, formatTime, parseTimeM)
import Database.MySQL.Simple (Connection)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)

-- | Value that can be typed in the console.
class FieldInput a where
  parseField :: String -> Maybe a
  showField :: a -> String

  -- | Format hint shown in the prompt.
  fieldHint :: Proxy a -> String
  fieldHint _ = ""

  -- | Value used for empty input; Nothing means the field is required.
  emptyValue :: Maybe a
  emptyValue = Nothing

instance FieldInput Int where
  parseField = readMaybe
  showField = show

instance FieldInput String where
  parseField s = if null s then Nothing else Just s
  showField = id

instance FieldInput TimeOfDay where
  parseField s = parseTimeM True defaultTimeLocale "%H:%M" s
  showField = formatTime defaultTimeLocale "%H:%M"
  fieldHint _ = "HH:MM"

instance FieldInput Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d"
  showField = formatTime defaultTimeLocale "%Y-%m-%d"
  fieldHint _ = "YYYY-MM-DD"

-- | Optional field: empty input or "-" means NULL.
instance FieldInput a => FieldInput (Maybe a) where
  parseField "-" = Just Nothing
  parseField s = Just <$> parseField s
  showField = maybe "-" showField
  fieldHint _ = let h = fieldHint (Proxy :: Proxy a) in if null h then "optional, - to clear" else h ++ ", optional"
  emptyValue = Just Nothing

-- | Helpers for enumerations.
parseEnum :: DbEnum a => String -> Maybe a
parseEnum = fromDb . map toLower

enumHint :: DbEnum a => Proxy a -> String
enumHint p = intercalate "/" (map toDb (values p))
  where
    values :: DbEnum a => Proxy a -> [a]
    values _ = allValues

-- | Raised to cancel the current operation with a message.
newtype UserAbort = UserAbort String deriving (Show)

instance Exception UserAbort

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | Print a prompt and read one line; end of input quits the program.
readLine :: String -> IO String
readLine prompt = do
  putStr prompt
  hFlush stdout
  eof <- isEOF
  if eof then putStrLn "" >> exitSuccess else trim <$> getLine

withHint :: forall a. FieldInput a => Proxy a -> String -> String
withHint p label = case fieldHint p of
  "" -> label
  h -> label ++ " (" ++ h ++ ")"

-- | Ask for a value until a valid one is entered.
ask :: forall a. FieldInput a => String -> IO a
ask label = do
  s <- readLine (withHint (Proxy :: Proxy a) label ++ ": ")
  case (s, emptyValue) of
    ("", Just v) -> pure v
    ("", Nothing) -> putStrLn "  Value is required." >> ask label
    _ -> maybe (putStrLn "  Invalid value, try again." >> ask label) pure (parseField s)

-- | Ask for a new value showing the current one; empty input keeps it.
askEdit :: forall a. FieldInput a => String -> a -> IO a
askEdit label old = do
  s <- readLine (withHint (Proxy :: Proxy a) label ++ " [" ++ showField old ++ "]: ")
  if null s
    then pure old
    else maybe (putStrLn "  Invalid value, try again." >> askEdit label old) pure (parseField s)

confirm :: String -> IO Bool
confirm question = do
  s <- readLine (question ++ " (y/n): ")
  pure (map toLower s `elem` ["y", "yes"])

-- | Numbered menu; returns the chosen number (0 = back / exit).
chooseMenu :: String -> String -> [String] -> IO Int
chooseMenu title zeroLabel items = do
  putStrLn ""
  putStrLn ("=== " ++ title ++ " ===")
  mapM_ putStrLn [show i ++ ". " ++ item | (i, item) <- zip [1 :: Int ..] items]
  putStrLn ("0. " ++ zeroLabel)
  let loop = do
        s <- readLine "> "
        case readMaybe s of
          Just n | n >= 0 && n <= length items -> pure n
          _ -> putStrLn "  Unknown option." >> loop
  loop

showOptions :: [(Int, String)] -> IO ()
showOptions opts = mapM_ (putStrLn . ("  " ++)) (wrap (map fmt opts))
  where
    fmt (i, s) = "[" ++ show i ++ "] " ++ s
    wrap = go []
    go acc [] = [unwords (reverse acc) | not (null acc)]
    go acc (x : xs)
      | not (null acc) && length (unwords (reverse (x : acc))) > 100 = unwords (reverse acc) : go [x] xs
      | otherwise = go (x : acc) xs

-- | Pick a row of another table by id (required foreign key).
chooseRef :: forall a. Referable a => Connection -> Proxy a -> String -> Maybe Int -> IO Int
chooseRef conn p label current = do
  opts <- refOptions conn p
  if null opts
    then throwIO (UserAbort ("Table " ++ tableName p ++ " is empty, add records there first."))
    else do
      putStrLn (label ++ " - available:")
      showOptions opts
      let loop = do
            i <- maybe (ask label) (askEdit label) current
            if i `elem` map fst opts
              then pure i
              else putStrLn ("  No " ++ entityName p ++ " with id " ++ show i ++ ".") >> loop
      loop

-- | Pick a row of another table by id or leave it empty (nullable foreign key).
chooseOptRef :: forall a. Referable a => Connection -> Proxy a -> String -> Maybe (Maybe Int) -> IO (Maybe Int)
chooseOptRef conn p label current = do
  opts <- refOptions conn p
  putStrLn (label ++ " - available:")
  showOptions opts
  let loop = do
        mi <- maybe (ask label) (askEdit label) current
        case mi of
          Just i | i `notElem` map fst opts ->
            putStrLn ("  No " ++ entityName p ++ " with id " ++ show i ++ ".") >> loop
          _ -> pure mi
  loop
