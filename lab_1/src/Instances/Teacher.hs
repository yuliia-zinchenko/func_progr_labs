{-# LANGUAGE OverloadedStrings #-}

module Instances.Teacher () where

import Classes
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple (Only (..), query, query_)
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Instances.User ()
import Form
import Input
import RowParser
import Types

instance QueryResults Teacher where
  convertResults = parseRow $ Teacher <$> field <*> field <*> field <*> field

instance QueryParams Teacher where
  renderParams t = [render (tchUserId t), render (tchDepartment t), render (tchPosition t)]

instance Entity Teacher where
  entityName _ = "teacher"
  tableName _ = "teachers"
  columns _ = ["user_id", "department", "position"]
  entityId = tchId

instance Repository Teacher where
  conflicts conn t = do
    roles <- query conn "SELECT role FROM users WHERE id = ?" (Only (tchUserId t))
    pure $ case roles of
      [Only role] -> issues [(role /= TeacherRole, "Selected user must have role 'teacher'.")]
      _ -> []

-- | Teachers are shown by the name of the linked user.
instance Referable Teacher where
  refLabel t = "user #" ++ show (tchUserId t)
  refOptions conn _ =
    query_ conn "SELECT t.id, u.full_name FROM teachers t JOIN users u ON u.id = t.user_id ORDER BY t.id"

instance Displayable Teacher where
  headers _ = ["ID", "User ID", "Department", "Position"]
  cells t = [show (tchId t), show (tchUserId t), tchDepartment t, tchPosition t]

instance Inputable Teacher where
  readNew conn =
    Teacher 0
      <$> chooseRef conn (Proxy :: Proxy User) "User ID" Nothing
      <*> ask "Department"
      <*> ask "Position"
  editFields conn t =
    Teacher (tchId t)
      <$> chooseRef conn (Proxy :: Proxy User) "User ID" (Just (tchUserId t))
      <*> askEdit "Department" (tchDepartment t)
      <*> askEdit "Position" (tchPosition t)

instance Validatable Teacher

instance WebForm Teacher where
  webForm =
    Teacher
      <$> formId
      <*> ref "user_id" "User" "users" tchUserId
      <*> input "department" "Department" tchDepartment
      <*> input "position" "Position" tchPosition
