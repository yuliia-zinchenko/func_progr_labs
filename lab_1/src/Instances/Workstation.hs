{-# LANGUAGE OverloadedStrings #-}

module Instances.Workstation () where

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
import RowParser
import Types

instance QueryResults Workstation where
  convertResults =
    parseRow $
      Workstation <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance QueryParams Workstation where
  renderParams w =
    [ render (wsClassroomId w)
    , render (wsSeat w)
    , render (wsInventory w)
    , render (wsCpu w)
    , render (wsRamGb w)
    , render (wsOs w)
    , render (wsStatus w)
    ]

instance Entity Workstation where
  entityName _ = "workstation"
  tableName _ = "workstations"
  columns _ = ["classroom_id", "seat_number", "inventory_number", "cpu", "ram_gb", "os", "status"]
  entityId = wsId

instance Repository Workstation where
  conflicts conn w = do
    capacity <- query conn "SELECT capacity FROM classrooms WHERE id = ?" (Only (wsClassroomId w))
    pure $ case capacity of
      [Only cap] -> issues [(wsSeat w > cap, "Seat number exceeds class capacity (" ++ show (cap :: Int) ++ ").")]
      _ -> []

instance Referable Workstation where
  refLabel = wsInventory

instance Displayable Workstation where
  headers _ = ["ID", "Class ID", "Seat", "Inventory No", "CPU", "RAM, GB", "OS", "Status"]
  cells w =
    [ show (wsId w)
    , show (wsClassroomId w)
    , show (wsSeat w)
    , wsInventory w
    , wsCpu w
    , show (wsRamGb w)
    , wsOs w
    , showField (wsStatus w)
    ]

instance Inputable Workstation where
  readNew conn =
    Workstation 0
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" Nothing
      <*> ask "Seat number"
      <*> ask "Inventory number"
      <*> ask "CPU"
      <*> ask "RAM, GB"
      <*> ask "Operating system"
      <*> ask "Status"
  editFields conn w =
    Workstation (wsId w)
      <$> chooseRef conn (Proxy :: Proxy Classroom) "Classroom ID" (Just (wsClassroomId w))
      <*> askEdit "Seat number" (wsSeat w)
      <*> askEdit "Inventory number" (wsInventory w)
      <*> askEdit "CPU" (wsCpu w)
      <*> askEdit "RAM, GB" (wsRamGb w)
      <*> askEdit "Operating system" (wsOs w)
      <*> askEdit "Status" (wsStatus w)

instance Validatable Workstation where
  validate w = do
    ensure (wsSeat w > 0) "Seat number must be positive."
    ensure (wsRamGb w > 0) "RAM size must be positive."
    pure w

instance WebForm Workstation where
  webForm =
    Workstation
      <$> formId
      <*> ref "classroom_id" "Classroom" "classrooms" wsClassroomId
      <*> input "seat_number" "Seat number" wsSeat
      <*> input "inventory_number" "Inventory number" wsInventory
      <*> input "cpu" "CPU" wsCpu
      <*> input "ram_gb" "RAM, GB" wsRamGb
      <*> input "os" "Operating system" wsOs
      <*> input "status" "Status" wsStatus
