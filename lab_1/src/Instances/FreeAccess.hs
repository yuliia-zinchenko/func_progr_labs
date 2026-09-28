{-# LANGUAGE OverloadedStrings #-}

module Instances.FreeAccess () where

import Classes
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple (Only (..), query)
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import Instances.Classroom ()
import Instances.Enums ()
import Instances.User ()
import RowParser
import Types

instance QueryResults FreeAccess where
  convertResults =
    parseRow $ FreeAccess <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance QueryParams FreeAccess where
  renderParams f =
    [ render (faClassroomId f)
    , render (faDay f)
    , render (faStart f)
    , render (faEnd f)
    , render (faDutyUserId f)
    , render (faNote f)
    ]

instance Entity FreeAccess where
  entityName _ = "free access window"
  tableName _ = "free_access"
  columns _ = ["classroom_id", "week_day", "start_time", "end_time", "duty_user_id", "note"]
  entityId = faId

-- | Free access must fit into working hours and must not overlap with lessons
-- or with another free access window of the same class.
instance Repository FreeAccess where
  conflicts conn f = do
    hours <- query conn "SELECT open_time, close_time FROM classrooms WHERE id = ?" (Only (faClassroomId f))
    let outside = case hours of
          [(open, close)] -> faStart f < open || faEnd f > close
          _ -> False
    lessons <-
      countRows
        conn
        "SELECT COUNT(*) FROM schedule WHERE classroom_id = ? AND week_day = ? AND start_time < ? AND end_time > ?"
        (faClassroomId f, faDay f, faEnd f, faStart f)
    windows <-
      countRows
        conn
        "SELECT COUNT(*) FROM free_access WHERE id <> ? AND classroom_id = ? AND week_day = ? AND start_time < ? AND end_time > ?"
        (faId f, faClassroomId f, faDay f, faEnd f, faStart f)
    pure $
      issues
        [ (outside, "Free access time is outside the class working hours.")
        , (lessons > 0, "Time overlaps with planned lessons in this class.")
        , (windows > 0, "Time overlaps with another free access window of this class.")
        ]

instance Displayable FreeAccess where
  headers _ = ["ID", "Class ID", "Day", "Start", "End", "Duty user ID", "Note"]
  cells f =
    [ show (faId f)
    , show (faClassroomId f)
    , showField (faDay f)
    , showField (faStart f)
    , showField (faEnd f)
    , showField (faDutyUserId f)
    , showField (faNote f)
    ]

instance Inputable FreeAccess where
  readNew conn =
    FreeAccess 0
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" Nothing
      <*> ask "Week day"
      <*> ask "Start time"
      <*> ask "End time"
      <*> chooseOptRef conn (Proxy :: Proxy User) "Duty user ID" Nothing
      <*> ask "Note"
  editFields conn f =
    FreeAccess (faId f)
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" (Just (faClassroomId f))
      <*> askEdit "Week day" (faDay f)
      <*> askEdit "Start time" (faStart f)
      <*> askEdit "End time" (faEnd f)
      <*> chooseOptRef conn (Proxy :: Proxy User) "Duty user ID" (Just (faDutyUserId f))
      <*> askEdit "Note" (faNote f)

instance Validatable FreeAccess where
  validate f = do
    ensure (faStart f < faEnd f) "Start time must be earlier than end time."
    pure f

instance WebForm FreeAccess where
  webForm =
    FreeAccess
      <$> formId
      <*> ref "classroom_id" "Classroom" "classrooms" faClassroomId
      <*> input "week_day" "Week day" faDay
      <*> input "start_time" "Start time" faStart
      <*> input "end_time" "End time" faEnd
      <*> optRef "duty_user_id" "On duty" "users" faDutyUserId
      <*> input "note" "Note" faNote
