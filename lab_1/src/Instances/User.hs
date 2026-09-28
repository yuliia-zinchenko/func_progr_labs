{-# LANGUAGE OverloadedStrings #-}

module Instances.User () where

import Classes
import Data.Maybe (isJust)
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import Instances.Enums ()
import RowParser
import Types

instance QueryResults User where
  convertResults = parseRow $ User <$> field <*> field <*> field <*> field <*> field <*> field

instance QueryParams User where
  renderParams u =
    [ render (usrFullName u)
    , render (usrRole u)
    , render (usrGroup u)
    , render (usrEmail u)
    , render (usrPhone u)
    ]

instance Entity User where
  entityName _ = "user"
  tableName _ = "users"
  columns _ = ["full_name", "role", "study_group", "email", "phone"]
  entityId = usrId

instance Repository User where
  conflicts conn u = do
    teacherRows <- countRows conn "SELECT COUNT(*) FROM teachers WHERE user_id = ?" [usrId u]
    pure $ issues [(teacherRows > 0 && usrRole u /= TeacherRole, "User is registered as a teacher, role must stay 'teacher'.")]

instance Referable User where
  refLabel u = usrFullName u ++ " (" ++ showField (usrRole u) ++ ")"

instance Displayable User where
  headers _ = ["ID", "Full name", "Role", "Group", "Email", "Phone"]
  cells u =
    [ show (usrId u)
    , usrFullName u
    , showField (usrRole u)
    , showField (usrGroup u)
    , usrEmail u
    , showField (usrPhone u)
    ]

instance Inputable User where
  readNew _ =
    User 0
      <$> ask "Full name"
      <*> ask "Role"
      <*> ask "Study group"
      <*> ask "Email"
      <*> ask "Phone"
  editFields _ u =
    User (usrId u)
      <$> askEdit "Full name" (usrFullName u)
      <*> askEdit "Role" (usrRole u)
      <*> askEdit "Study group" (usrGroup u)
      <*> askEdit "Email" (usrEmail u)
      <*> askEdit "Phone" (usrPhone u)

instance Validatable User where
  validate u = do
    ensure ('@' `elem` usrEmail u) "Email must contain '@'."
    ensure (usrRole u /= Student || isJust (usrGroup u)) "Student must have a study group."
    pure u

instance WebForm User where
  webForm =
    User
      <$> formId
      <*> input "full_name" "Full name" usrFullName
      <*> input "role" "Role" usrRole
      <*> input "study_group" "Study group" usrGroup
      <*> input "email" "Email" usrEmail
      <*> input "phone" "Phone" usrPhone
