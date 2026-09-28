-- | Instances for enumerations: database mapping ('DbEnum'), SQL parameters ('Param'),
-- SQL results ('Result'), console input ('FieldInput') and web forms ('WebField').
module Instances.Enums () where

import Classes
import Control.Exception (throw)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Database.MySQL.Base.Types (Field (..))
import Database.MySQL.Simple.Param (Action, Param (..))
import Database.MySQL.Simple.Result (Result (..), ResultError (..))
import Form (FieldKind (..), WebField (..))
import Input
import Types

instance DbEnum Role where
  toDb Student = "student"
  toDb TeacherRole = "teacher"
  toDb Admin = "admin"

instance DbEnum WeekDay where
  toDb Mon = "mon"
  toDb Tue = "tue"
  toDb Wed = "wed"
  toDb Thu = "thu"
  toDb Fri = "fri"
  toDb Sat = "sat"
  toDb Sun = "sun"

instance DbEnum WsStatus where
  toDb Working = "working"
  toDb Repair = "repair"
  toDb Off = "off"

instance DbEnum LessonType where
  toDb Lecture = "lecture"
  toDb Lab = "lab"
  toDb Practice = "practice"

renderEnum :: DbEnum a => a -> Action
renderEnum = render . toDb

convertEnum :: forall a. (DbEnum a, Typeable a) => Field -> Maybe ByteString -> a
convertEnum f v = case fromDb (convert f v) of
  Just x -> x
  Nothing ->
    throw $
      ConversionFailed
        (show (fieldType f))
        (show (typeRep (Proxy :: Proxy a)))
        (B8.unpack (fieldName f))
        "unknown enum value"

instance Param Role where render = renderEnum
instance Param WeekDay where render = renderEnum
instance Param WsStatus where render = renderEnum
instance Param LessonType where render = renderEnum

instance Result Role where convert = convertEnum
instance Result WeekDay where convert = convertEnum
instance Result WsStatus where convert = convertEnum
instance Result LessonType where convert = convertEnum

instance FieldInput Role where
  parseField = parseEnum
  showField = toDb
  fieldHint = enumHint

instance FieldInput WeekDay where
  parseField = parseEnum
  showField = toDb
  fieldHint = enumHint

instance FieldInput WsStatus where
  parseField = parseEnum
  showField = toDb
  fieldHint = enumHint

instance FieldInput LessonType where
  parseField = parseEnum
  showField = toDb
  fieldHint = enumHint

enumKind :: forall a. DbEnum a => Proxy a -> FieldKind
enumKind _ = KEnum (map toDb (allValues :: [a]))

instance WebField Role where webKind = enumKind
instance WebField WeekDay where webKind = enumKind
instance WebField WsStatus where webKind = enumKind
instance WebField LessonType where webKind = enumKind
