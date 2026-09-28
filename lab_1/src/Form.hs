-- | Web forms. A 'Form' is an applicative description of a record: from one definition
-- we get the list of fields (for building the HTML form), the current values of a record
-- (for editing) and a parser of submitted values that collects all errors at once.
module Form
  ( FieldKind (..)
  , FieldSpec (..)
  , Form
  , Values
  , WebField (..)
  , WebForm (..)
  , formId
  , input
  , ref
  , optRef
  , formFields
  , formValues
  , parseForm
  ) where

import Classes (Entity)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Time (Day, TimeOfDay)
import Input (FieldInput (..))
import Text.Read (readMaybe)

data FieldKind
  = KText
  | KNumber
  | KTime
  | KDate
  | KEnum [String]
  | -- | foreign key; the argument is the referenced table
    KRef String

-- | One form field of record type @r@.
data FieldSpec r = FieldSpec
  { fsName     :: String
  , fsLabel    :: String
  , fsKind     :: FieldKind
  , fsRequired :: Bool
  , fsValue    :: r -> String
  }

-- | Submitted form: field name -> raw text.
type Values = Map.Map String String

-- | Fields of record @r@ and a parser producing @a@.
data Form r a = Form [FieldSpec r] (Values -> Either [String] a)

instance Functor (Form r) where
  fmap f (Form specs p) = Form specs (fmap f . p)

-- | Unlike 'Either', errors of all fields are accumulated.
instance Applicative (Form r) where
  pure x = Form [] (const (Right x))
  Form s1 pf <*> Form s2 pa = Form (s1 ++ s2) $ \vs -> case (pf vs, pa vs) of
    (Right f, Right a) -> Right (f a)
    (Left e1, Left e2) -> Left (e1 ++ e2)
    (Left e1, _) -> Left e1
    (_, Left e2) -> Left e2

-- | Value that can be edited in an HTML form.
class FieldInput a => WebField a where
  webKind :: Proxy a -> FieldKind

  webRequired :: Proxy a -> Bool
  webRequired _ = True

  -- | Value shown in the form ("" means empty).
  webValue :: a -> String
  webValue = showField

instance WebField Int where webKind _ = KNumber
instance WebField String where webKind _ = KText
instance WebField TimeOfDay where webKind _ = KTime
instance WebField Day where webKind _ = KDate

instance WebField a => WebField (Maybe a) where
  webKind _ = webKind (Proxy :: Proxy a)
  webRequired _ = False
  webValue = maybe "" webValue

-- | Record type with a web form. Field names are the table columns.
class Entity a => WebForm a where
  webForm :: Form a a

-- | Record id: taken from the hidden "id" value, 0 for a new record.
formId :: Form r Int
formId = Form [] (\vs -> Right (maybe 0 id (Map.lookup "id" vs >>= readMaybe)))

-- | Ordinary field.
input :: forall r a. WebField a => String -> String -> (r -> a) -> Form r a
input name label getter = Form [spec] parse
  where
    p = Proxy :: Proxy a
    spec = FieldSpec name label (webKind p) (webRequired p) (webValue . getter)
    parse vs = case (trim (Map.findWithDefault "" name vs), emptyValue) of
      ("", Just v) -> Right v
      ("", Nothing) -> Left [label ++ " is required."]
      (raw, _) -> maybe (Left [label ++ ": invalid value \"" ++ raw ++ "\"."]) Right (parseField raw)

-- | Required foreign key to table @target@.
ref :: String -> String -> String -> (r -> Int) -> Form r Int
ref name label target getter = withKind (KRef target) (input name label getter)

-- | Optional foreign key to table @target@.
optRef :: String -> String -> String -> (r -> Maybe Int) -> Form r (Maybe Int)
optRef name label target getter = withKind (KRef target) (input name label getter)

withKind :: FieldKind -> Form r a -> Form r a
withKind k (Form specs p) = Form [s {fsKind = k} | s <- specs] p

formFields :: forall a. WebForm a => Proxy a -> [FieldSpec a]
formFields _ = let Form specs _ = (webForm :: Form a a) in specs

formValues :: forall a. WebForm a => a -> [(String, String)]
formValues x = [(fsName s, fsValue s x) | s <- formFields (Proxy :: Proxy a)]

parseForm :: WebForm a => Values -> Either [String] a
parseForm vs = let Form _ p = webForm in p vs

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
