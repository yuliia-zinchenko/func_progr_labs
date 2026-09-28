{-# LANGUAGE OverloadedStrings #-}

module Instances.Classroom () where

import Classes
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import RowParser
import Types

instance QueryResults Classroom where
  convertResults =
    parseRow $
      Classroom <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance QueryParams Classroom where
  renderParams c =
    [ render (crRoom c)
    , render (crBuilding c)
    , render (crFloor c)
    , render (crCapacity c)
    , render (crOpen c)
    , render (crClose c)
    , render (crDescription c)
    ]

instance Entity Classroom where
  entityName _ = "classroom"
  tableName _ = "classrooms"
  columns _ = ["room_number", "building", "floor", "capacity", "open_time", "close_time", "description"]
  entityId = crId

instance Repository Classroom where
  conflicts conn c = do
    seatsOutside <-
      countRows conn "SELECT COUNT(*) FROM workstations WHERE classroom_id = ? AND seat_number > ?" (crId c, crCapacity c)
    pure $ issues [(seatsOutside > 0, "Class has workstations with seat number greater than the new capacity.")]

instance Referable Classroom where
  refLabel c = crRoom c ++ " (" ++ crBuilding c ++ ")"

instance Displayable Classroom where
  headers _ = ["ID", "Room", "Building", "Floor", "Seats", "Open", "Close", "Description"]
  cells c =
    [ show (crId c)
    , crRoom c
    , crBuilding c
    , show (crFloor c)
    , show (crCapacity c)
    , showField (crOpen c)
    , showField (crClose c)
    , showField (crDescription c)
    ]

instance Inputable Classroom where
  readNew _ =
    Classroom 0
      <$> ask "Room number"
      <*> ask "Building"
      <*> ask "Floor"
      <*> ask "Capacity (seats)"
      <*> ask "Opens at"
      <*> ask "Closes at"
      <*> ask "Description"
  editFields _ c =
    Classroom (crId c)
      <$> askEdit "Room number" (crRoom c)
      <*> askEdit "Building" (crBuilding c)
      <*> askEdit "Floor" (crFloor c)
      <*> askEdit "Capacity (seats)" (crCapacity c)
      <*> askEdit "Opens at" (crOpen c)
      <*> askEdit "Closes at" (crClose c)
      <*> askEdit "Description" (crDescription c)

instance Validatable Classroom where
  validate c = do
    ensure (crCapacity c > 0) "Capacity must be positive."
    ensure (crFloor c >= -2 && crFloor c <= 50) "Floor must be between -2 and 50."
    ensure (crOpen c < crClose c) "Opening time must be earlier than closing time."
    pure c

instance WebForm Classroom where
  webForm =
    Classroom
      <$> formId
      <*> input "room_number" "Room number" crRoom
      <*> input "building" "Building" crBuilding
      <*> input "floor" "Floor" crFloor
      <*> input "capacity" "Capacity (seats)" crCapacity
      <*> input "open_time" "Opens at" crOpen
      <*> input "close_time" "Closes at" crClose
      <*> input "description" "Description" crDescription
