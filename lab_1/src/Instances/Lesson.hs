{-# LANGUAGE OverloadedStrings #-}

module Instances.Lesson () where

import Classes
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple (Only (..), query)
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import Instances.Classroom ()
import Instances.Discipline ()
import Instances.Enums ()
import Instances.Teacher ()
import RowParser
import Types

instance QueryResults Lesson where
  convertResults =
    parseRow $
      Lesson <$> field <*> field <*> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field

instance QueryParams Lesson where
  renderParams l =
    [ render (lsClassroomId l)
    , render (lsTeacherId l)
    , render (lsDisciplineId l)
    , render (lsGroup l)
    , render (lsDay l)
    , render (lsStart l)
    , render (lsEnd l)
    , render (lsType l)
    , render (lsValidFrom l)
    , render (lsValidTo l)
    ]

instance Entity Lesson where
  entityName _ = "lesson"
  tableName _ = "schedule"
  columns _ =
    [ "classroom_id", "teacher_id", "discipline_id", "study_group", "week_day"
    , "start_time", "end_time", "lesson_type", "valid_from", "valid_to"
    ]
  entityId = lsId

-- | A lesson must fit into the class working hours and must not overlap (on the same
-- week day, in the same period) with another lesson of the same class, teacher or group,
-- or with free access time of the class.
instance Repository Lesson where
  conflicts conn l = do
    hours <- query conn "SELECT open_time, close_time FROM classrooms WHERE id = ?" (Only (lsClassroomId l))
    let outside = case hours of
          [(open, close)] -> lsStart l < open || lsEnd l > close
          _ -> False
        overlapping column value =
          countRows
            conn
            ( "SELECT COUNT(*) FROM schedule WHERE id <> ? AND " <> column <> " = ? AND week_day = ?"
                <> " AND start_time < ? AND end_time > ? AND valid_from <= ? AND valid_to >= ?"
            )
            [render (lsId l), value, render (lsDay l), render (lsEnd l), render (lsStart l), render (lsValidTo l), render (lsValidFrom l)]
    roomBusy <- overlapping "classroom_id" (render (lsClassroomId l))
    teacherBusy <- overlapping "teacher_id" (render (lsTeacherId l))
    groupBusy <- overlapping "study_group" (render (lsGroup l))
    freeAccess <-
      countRows
        conn
        "SELECT COUNT(*) FROM free_access WHERE classroom_id = ? AND week_day = ? AND start_time < ? AND end_time > ?"
        (lsClassroomId l, lsDay l, lsEnd l, lsStart l)
    pure $
      issues
        [ (outside, "Lesson is outside the class working hours.")
        , (roomBusy > 0, "Class is already occupied by another lesson at this time.")
        , (teacherBusy > 0, "Teacher already has another lesson at this time.")
        , (groupBusy > 0, "Group already has another lesson at this time.")
        , (freeAccess > 0, "Time overlaps with free access hours of the class.")
        ]

instance Displayable Lesson where
  headers _ = ["ID", "Class ID", "Teacher ID", "Discipline ID", "Group", "Day", "Start", "End", "Type", "From", "To"]
  cells l =
    [ show (lsId l)
    , show (lsClassroomId l)
    , show (lsTeacherId l)
    , show (lsDisciplineId l)
    , lsGroup l
    , showField (lsDay l)
    , showField (lsStart l)
    , showField (lsEnd l)
    , showField (lsType l)
    , showField (lsValidFrom l)
    , showField (lsValidTo l)
    ]

instance Inputable Lesson where
  readNew conn =
    Lesson 0
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" Nothing
      <*> chooseRef conn (Proxy :: Proxy Teacher) "Teacher ID" Nothing
      <*> chooseRef conn (Proxy :: Proxy Discipline) "Discipline ID" Nothing
      <*> ask "Study group"
      <*> ask "Week day"
      <*> ask "Start time"
      <*> ask "End time"
      <*> ask "Lesson type"
      <*> ask "Valid from"
      <*> ask "Valid to"
  editFields conn l =
    Lesson (lsId l)
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" (Just (lsClassroomId l))
      <*> chooseRef conn (Proxy :: Proxy Teacher) "Teacher ID" (Just (lsTeacherId l))
      <*> chooseRef conn (Proxy :: Proxy Discipline) "Discipline ID" (Just (lsDisciplineId l))
      <*> askEdit "Study group" (lsGroup l)
      <*> askEdit "Week day" (lsDay l)
      <*> askEdit "Start time" (lsStart l)
      <*> askEdit "End time" (lsEnd l)
      <*> askEdit "Lesson type" (lsType l)
      <*> askEdit "Valid from" (lsValidFrom l)
      <*> askEdit "Valid to" (lsValidTo l)

instance Validatable Lesson where
  validate l = do
    ensure (lsStart l < lsEnd l) "Start time must be earlier than end time."
    ensure (lsValidFrom l <= lsValidTo l) "Period start must not be later than period end."
    pure l

instance WebForm Lesson where
  webForm =
    Lesson
      <$> formId
      <*> ref "classroom_id" "Classroom" "classrooms" lsClassroomId
      <*> ref "teacher_id" "Teacher" "teachers" lsTeacherId
      <*> ref "discipline_id" "Discipline" "disciplines" lsDisciplineId
      <*> input "study_group" "Study group" lsGroup
      <*> input "week_day" "Week day" lsDay
      <*> input "start_time" "Start time" lsStart
      <*> input "end_time" "End time" lsEnd
      <*> input "lesson_type" "Lesson type" lsType
      <*> input "valid_from" "Valid from" lsValidFrom
      <*> input "valid_to" "Valid to" lsValidTo
