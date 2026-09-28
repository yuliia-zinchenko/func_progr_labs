module Instances.Discipline () where

import Classes
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Form
import Input
import RowParser
import Types

instance QueryResults Discipline where
  convertResults = parseRow $ Discipline <$> field <*> field <*> field

instance QueryParams Discipline where
  renderParams d = [render (dsName d), render (dsHours d)]

instance Entity Discipline where
  entityName _ = "discipline"
  tableName _ = "disciplines"
  columns _ = ["name", "hours"]
  entityId = dsId

instance Repository Discipline

instance Referable Discipline where
  refLabel = dsName

instance Displayable Discipline where
  headers _ = ["ID", "Name", "Hours"]
  cells d = [show (dsId d), dsName d, show (dsHours d)]

instance Inputable Discipline where
  readNew _ = Discipline 0 <$> ask "Name" <*> ask "Hours"
  editFields _ d = Discipline (dsId d) <$> askEdit "Name" (dsName d) <*> askEdit "Hours" (dsHours d)

instance Validatable Discipline where
  validate d = do
    ensure (dsHours d > 0) "Hours must be positive."
    pure d

instance WebForm Discipline where
  webForm = Discipline <$> formId <*> input "name" "Name" dsName <*> input "hours" "Hours" dsHours
