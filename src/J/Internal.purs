module JSON.Internal where

import Prelude

import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn7, runFn2, runFn7)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

-- | A type that represents all varieties of JSON value.
-- |
-- | This is not a PureScript sum type, instead the underlying JSON representation is used for
-- | efficiency and performance reasons.
foreign import data JSON :: Type

instance Eq JSON where
  eq a b =
    runFn7 _case
      (\x -> runFn7 _case (eq x) _false _false _false _false _false a)
      (\x -> runFn7 _case _false (eq x) _false _false _false _false a)
      (\x -> runFn7 _case _false _false (eq x) _false _false _false a)
      (\x -> runFn7 _case _false _false _false (eq x) _false _false a)
      (\x -> runFn7 _case _false _false _false _false (eq x) _false a)
      (\x -> runFn7 _case _false _false _false _false _false (eq x) a)
      b

_false :: forall a. a -> Boolean
_false _ = false

instance Ord JSON where
  compare a b =
    runFn7 _case
      (\x -> runFn7 _case (compare x) _gt _gt _gt _gt _gt b)
      (\x -> runFn7 _case _lt (compare x) _gt _gt _gt _gt b)
      (\x -> runFn7 _case _lt _lt (compare x) _gt _gt _gt b)
      (\x -> runFn7 _case _lt _lt _lt (compare x) _gt _gt b)
      (\x -> runFn7 _case _lt _lt _lt _lt (compare x) _gt b)
      (\x -> runFn7 _case _lt _lt _lt _lt _lt (compare x) b)
      a

_lt :: forall a. a -> Ordering
_lt _ = LT

_gt :: forall a. a -> Ordering
_gt _ = GT

-- | A type that represents JSON objects. Similar to the JSON type, this is not a PureScript type,
-- | but represents the underlying representation for JSON objects.
foreign import data Object :: Type

instance Eq Object where
  eq x y = eq (runFn2 _entries Tuple x) (runFn2 _entries Tuple y)

instance Ord Object where
  compare x y = compare (runFn2 _entries Tuple x) (runFn2 _entries Tuple y)

foreign import _parse
  :: Fn3
       (forall a b. a -> Either a b)
       (forall a b. b -> Either a b)
       String
       (Either String JSON)

foreign import _fromNumberWithDefault :: Fn2 Int Number JSON

foreign import _case
  :: forall a
   . Fn7
       (Unit -> a)
       (Boolean -> a)
       (Number -> a)
       (String -> a)
       (Array JSON -> a)
       (Object -> a)
       JSON
       a

foreign import _insert :: Fn3 String JSON Object Object

foreign import _delete :: Fn2 String Object Object

foreign import _fromEntries
  :: Fn3
       (forall x y. Tuple x y -> x)
       (forall x y. Tuple x y -> y)
       (Array (Tuple String JSON))
       Object

foreign import _entries :: forall c. Fn2 (String -> JSON -> c) Object (Array c)

foreign import _lookup
  :: Fn4
       (forall a. Maybe a)
       (forall a. a -> Maybe a)
       String
       Object
       (Maybe JSON)
