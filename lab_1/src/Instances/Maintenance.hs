module Instances.Maintenance () where

import Classes
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import Instances.User ()
import Instances.Workstation ()
import RowParser
import Types

instance QueryResults Maintenance where
  convertResults = parseRow $ Maintenance <$> field <*> field <*> field <*> field <*> field

instance QueryParams Maintenance where
  renderParams m =
    [render (mtWorkstationId m), render (mtDate m), render (mtDescription m), render (mtPerformedBy m)]

instance Entity Maintenance where
  entityName _ = "maintenance record"
  tableName _ = "maintenance_log"
  columns _ = ["workstation_id", "log_date", "description", "performed_by"]
  entityId = mtId

instance Repository Maintenance

instance Displayable Maintenance where
  headers _ = ["ID", "Workstation ID", "Date", "Description", "Performed by (user ID)"]
  cells m =
    [ show (mtId m)
    , show (mtWorkstationId m)
    , showField (mtDate m)
    , mtDescription m
    , showField (mtPerformedBy m)
    ]

instance Inputable Maintenance where
  readNew conn =
    Maintenance 0
      <$> chooseRef conn (Proxy :: Proxy Workstation) "Workstation ID" Nothing
      <*> ask "Date"
      <*> ask "Description"
      <*> chooseOptRef conn (Proxy :: Proxy User) "Performed by (user ID)" Nothing
  editFields conn m =
    Maintenance (mtId m)
      <$> chooseRef conn (Proxy :: Proxy Workstation) "Workstation ID" (Just (mtWorkstationId m))
      <*> askEdit "Date" (mtDate m)
      <*> askEdit "Description" (mtDescription m)
      <*> chooseOptRef conn (Proxy :: Proxy User) "Performed by (user ID)" (Just (mtPerformedBy m))

instance Validatable Maintenance

instance WebForm Maintenance where
  webForm =
    Maintenance
      <$> formId
      <*> ref "workstation_id" "Workstation" "workstations" mtWorkstationId
      <*> input "log_date" "Date" mtDate
      <*> input "description" "Description" mtDescription
      <*> optRef "performed_by" "Performed by" "users" mtPerformedBy
