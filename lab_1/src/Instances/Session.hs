{-# LANGUAGE OverloadedStrings #-}

module Instances.Session () where

import Classes
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple (Only (..), query)
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import Instances.Enums ()
import Instances.User ()
import Instances.Workstation ()
import RowParser
import Types

instance QueryResults Session where
  convertResults =
    parseRow $ Session <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance QueryParams Session where
  renderParams s =
    [ render (ssWorkstationId s)
    , render (ssUserId s)
    , render (ssDate s)
    , render (ssStart s)
    , render (ssEnd s)
    , render (ssPurpose s)
    ]

instance Entity Session where
  entityName _ = "workstation session"
  tableName _ = "workstation_sessions"
  columns _ = ["workstation_id", "user_id", "session_date", "start_time", "end_time", "purpose"]
  entityId = ssId

-- | A session is possible only on a working workstation, and neither the workstation
-- nor the user can have two sessions at the same time.
instance Repository Session where
  conflicts conn s = do
    status <- query conn "SELECT status FROM workstations WHERE id = ?" (Only (ssWorkstationId s))
    let broken = case status of
          [Only st] -> st /= Working
          _ -> False
        overlapping column value =
          countRows
            conn
            ( "SELECT COUNT(*) FROM workstation_sessions WHERE id <> ? AND " <> column
                <> " = ? AND session_date = ? AND start_time < ? AND end_time > ?"
            )
            (ssId s, value, ssDate s, ssEnd s, ssStart s)
    wsBusy <- overlapping "workstation_id" (ssWorkstationId s)
    userBusy <- overlapping "user_id" (ssUserId s)
    pure $
      issues
        [ (broken, "Workstation is not in 'working' state.")
        , (wsBusy > 0, "Workstation is already used by someone at this time.")
        , (userBusy > 0, "User already has another session at this time.")
        ]

instance Displayable Session where
  headers _ = ["ID", "Workstation ID", "User ID", "Date", "Start", "End", "Purpose"]
  cells s =
    [ show (ssId s)
    , show (ssWorkstationId s)
    , show (ssUserId s)
    , showField (ssDate s)
    , showField (ssStart s)
    , showField (ssEnd s)
    , showField (ssPurpose s)
    ]

instance Inputable Session where
  readNew conn =
    Session 0
      <$> chooseRef conn (Proxy :: Proxy Workstation) "Workstation ID" Nothing
      <*> chooseRef conn (Proxy :: Proxy User) "User ID" Nothing
      <*> ask "Date"
      <*> ask "Start time"
      <*> ask "End time"
      <*> ask "Purpose"
  editFields conn s =
    Session (ssId s)
      <$> chooseRef conn (Proxy :: Proxy Workstation) "Workstation ID" (Just (ssWorkstationId s))
      <*> chooseRef conn (Proxy :: Proxy User) "User ID" (Just (ssUserId s))
      <*> askEdit "Date" (ssDate s)
      <*> askEdit "Start time" (ssStart s)
      <*> askEdit "End time" (ssEnd s)
      <*> askEdit "Purpose" (ssPurpose s)

instance Validatable Session where
  validate s = do
    ensure (ssStart s < ssEnd s) "Start time must be earlier than end time."
    pure s

instance WebForm Session where
  webForm =
    Session
      <$> formId
      <*> ref "workstation_id" "Workstation" "workstations" ssWorkstationId
      <*> ref "user_id" "User" "users" ssUserId
      <*> input "session_date" "Date" ssDate
      <*> input "start_time" "Start time" ssStart
      <*> input "end_time" "End time" ssEnd
      <*> input "purpose" "Purpose" ssPurpose
