-- | All tables of the system in one place, shared by the console and the web interface.
module Registry
  ( TableSpec (..)
  , RefSpec (..)
  , tables
  , lookupTable
  , lookupRef
  ) where

import Classes
import Data.List (find)
import Data.Proxy (Proxy (..))
import Form (WebForm)
import Instances.Classroom ()
import Instances.Discipline ()
import Instances.FreeAccess ()
import Instances.Lesson ()
import Instances.Maintenance ()
import Instances.Session ()
import Instances.Teacher ()
import Instances.User ()
import Instances.Workstation ()
import Types

-- | Table type packed together with its instances and a title for menus.
data TableSpec
  = forall a. (Repository a, Displayable a, Inputable a, Validatable a, WebForm a) => TableSpec String (Proxy a)

-- | Table that other tables refer to (can be chosen in a foreign key field).
data RefSpec = forall a. Referable a => RefSpec (Proxy a)

tables :: [TableSpec]
tables =
  [ TableSpec "Classrooms" (Proxy :: Proxy Classroom)
  , TableSpec "Workstations" (Proxy :: Proxy Workstation)
  , TableSpec "Users" (Proxy :: Proxy User)
  , TableSpec "Teachers" (Proxy :: Proxy Teacher)
  , TableSpec "Disciplines" (Proxy :: Proxy Discipline)
  , TableSpec "Schedule (planned lessons)" (Proxy :: Proxy Lesson)
  , TableSpec "Free access time" (Proxy :: Proxy FreeAccess)
  , TableSpec "Workstation sessions (usage)" (Proxy :: Proxy Session)
  , TableSpec "Maintenance log" (Proxy :: Proxy Maintenance)
  ]

refs :: [RefSpec]
refs =
  [ RefSpec (Proxy :: Proxy Classroom)
  , RefSpec (Proxy :: Proxy Workstation)
  , RefSpec (Proxy :: Proxy User)
  , RefSpec (Proxy :: Proxy Teacher)
  , RefSpec (Proxy :: Proxy Discipline)
  ]

-- | Find a table by its database name.
lookupTable :: String -> Maybe TableSpec
lookupTable name = find (\(TableSpec _ p) -> tableName p == name) tables

lookupRef :: String -> Maybe RefSpec
lookupRef name = find (\(RefSpec p) -> tableName p == name) refs
