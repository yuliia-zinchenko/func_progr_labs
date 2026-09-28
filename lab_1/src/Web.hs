{-# LANGUAGE OverloadedStrings #-}

-- | Web interface: JSON API over the same classes the console uses, plus static files
-- of the single-page frontend from the `web/` directory.
module Web (runWeb) where

import Classes
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, evaluate, throwIO)
import Control.Monad (forever, join, unless, void)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Database.MySQL.Simple (Connection)
import Errors (errorMessage, isFatal)
import Form
import Network.HTTP.Types.Status (status400, status404)
import Registry
import Reports
import System.Directory (doesFileExist)
import Web.Scotty (ActionM, ScottyM)
import qualified Web.Scotty as S

type Result = Either [String] Value

runWeb :: Int -> Connection -> IO ()
runWeb port conn = do
  ok <- doesFileExist "web/index.html"
  unless ok $ putStrLn "Warning: web/index.html not found, start the program from the project directory."
  jobs <- newChan
  putStrLn ("Web interface: http://localhost:" ++ show port ++ "  (Ctrl+C to stop)")
  void (forkIO (S.scotty port (routes (Db jobs conn))))
  -- The MySQL C client must be used from the thread that opened the connection,
  -- so all database work is executed here, in the main thread, one job at a time.
  forever (join (readChan jobs))

-- | Queue of database jobs for the main thread.
data Db = Db (Chan (IO ())) Connection

-- | Run a database action in the main thread and wait for its result.
-- The JSON is fully evaluated there, so conversion errors are caught too.
withDb :: Db -> (Connection -> IO Result) -> IO Result
withDb (Db jobs conn) act = do
  answer <- newEmptyMVar
  writeChan jobs (attempt >>= putMVar answer)
  takeMVar answer
  where
    attempt =
      (act conn >>= either (pure . Left) (\v -> evaluate (BL.length (encode v)) >> pure (Right v)))
        `catch` \(e :: SomeException) -> if isFatal e then throwIO e else pure (Left [errorMessage e])

respond :: Result -> ActionM ()
respond (Right v) = S.json v
respond (Left errs) = S.status status400 >> S.json (object ["errors" .= errs])

notFound :: ActionM ()
notFound = S.status status404 >> S.json (object ["errors" .= ["Not found." :: String]])

routes :: Db -> ScottyM ()
routes db = do
  static "/" "web/index.html" "text/html; charset=utf-8"
  static "/app.js" "web/app.js" "application/javascript; charset=utf-8"
  static "/style.css" "web/style.css" "text/css; charset=utf-8"

  S.get "/api/meta" $ run (fmap Right . meta)

  S.get "/api/tables/:table" $ withTable $ \(TableSpec _ p) -> run (\c -> Right <$> listRows c p)
  S.post "/api/tables/:table" $ withTable $ \(TableSpec _ p) -> do
    vs <- S.jsonData
    run (\c -> saveRecord c p Nothing vs)
  S.put "/api/tables/:table/:id" $ withTable $ \(TableSpec _ p) -> do
    i <- S.pathParam "id"
    vs <- S.jsonData
    run (\c -> saveRecord c p (Just i) vs)
  S.delete "/api/tables/:table/:id" $ withTable $ \(TableSpec _ p) -> do
    i <- S.pathParam "id"
    run $ \c -> do
      deleted <- remove p c i
      pure (if deleted then Right (object ["deleted" .= i]) else Left ["Record not found."])

  S.get "/api/options/:table" $ do
    name <- S.pathParam "table"
    case lookupRef name of
      Nothing -> notFound
      Just (RefSpec p) -> run $ \c -> do
        opts <- refOptions c p
        pure (Right (toJSON [object ["id" .= i, "label" .= l] | (i, l) <- opts]))

  S.get "/api/reports/:report" $ do
    key <- S.pathParam "report"
    params <- S.queryParams
    case lookupReport key of
      Nothing -> notFound
      Just r -> run $ \c -> do
        results <- reportRun r c (Map.fromList [(T.unpack k, T.unpack v) | (k, v) <- params])
        pure (Right (object ["tables" .= map tableJson results]))
  where
    run act = S.liftIO (withDb db act) >>= respond

    static route path contentType = S.get route $ do
      S.setHeader "Content-Type" contentType
      S.file path

    withTable k = do
      name <- S.pathParam "table"
      maybe notFound k (lookupTable name)

-- | Description of all tables and reports for building the interface.
meta :: Connection -> IO Value
meta conn = do
  rs <- mapM reportMeta reports
  pure (object ["tables" .= map tableMeta tables, "reports" .= rs])
  where
    reportMeta r = do
      ps <- mapM paramMeta (reportParams r)
      pure (object ["key" .= reportKey r, "title" .= reportTitle r, "params" .= ps])
    paramMeta prm = do
      extra <- case rpKind prm of
        PRef target -> pure (kind "ref" ++ ["ref" .= target])
        PDate -> pure (kind "date")
        PTime -> pure (kind "time")
        PChoice options -> (\os -> kind "choice" ++ ["options" .= os]) <$> options conn
      pure (object (["name" .= rpName prm, "label" .= rpLabel prm, "default" .= rpDefault prm] ++ extra))

tableMeta :: TableSpec -> Value
tableMeta (TableSpec title p) =
  object
    [ "key" .= tableName p
    , "title" .= title
    , "entity" .= entityName p
    , "headers" .= headers p
    , "fields" .= map fieldMeta (formFields p)
    ]

fieldMeta :: FieldSpec r -> Value
fieldMeta s =
  object (["name" .= fsName s, "label" .= fsLabel s, "required" .= fsRequired s] ++ kindMeta (fsKind s))
  where
    kindMeta KText = kind "text"
    kindMeta KNumber = kind "number"
    kindMeta KTime = kind "time"
    kindMeta KDate = kind "date"
    kindMeta (KEnum os) = kind "enum" ++ ["options" .= os]
    kindMeta (KRef target) = kind "ref" ++ ["ref" .= target]

kind :: String -> [Pair]
kind k = ["kind" .= k]

listRows :: forall a. (Repository a, Displayable a, WebForm a) => Connection -> Proxy a -> IO Value
listRows conn _ = do
  xs <- findAll conn :: IO [a]
  pure $
    object
      [ "rows"
          .= [ object ["id" .= entityId x, "cells" .= cells x, "values" .= Map.fromList (formValues x)]
             | x <- xs
             ]
      ]

-- | Create (no id) or update (with id) a record: parse the form, check it, write it.
saveRecord :: forall a. (Repository a, Validatable a, WebForm a) => Connection -> Proxy a -> Maybe Int -> Values -> IO Result
saveRecord conn _ mid vs =
  case parseForm (maybe (Map.delete "id" vs) (\i -> Map.insert "id" (show i) vs) mid) :: Either [String] a of
    Left errs -> pure (Left errs)
    Right x -> do
      problems <- checkRecord conn x
      if not (null problems)
        then pure (Left problems)
        else case mid of
          Nothing -> do
            i <- insert conn x
            pure (Right (object ["id" .= i]))
          Just i -> do
            existing <- findById conn i :: IO (Maybe a)
            case existing of
              Nothing -> pure (Left ["Record not found."])
              Just _ -> update conn x >> pure (Right (object ["id" .= i]))

tableJson :: ReportTable -> Value
tableJson t =
  object
    [ "title" .= rtTitle t
    , "headers" .= rtHeaders t
    , "rows" .= rtRows t
    , "bar" .= rtBar t
    , "empty" .= rtEmpty t
    ]
