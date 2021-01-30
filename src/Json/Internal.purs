module Json.Internal
  ( Json
  , Object
  , Null

  , parse
  , print
  , printIndented

  , case_
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject

  , null
  , fromNumber
  , fromNumberWithDefault
  , fromInt
  , fromBoolean
  , fromString
  , fromArray
  , fromObject

  , entries
  , keys
  , values
  , lookup
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function (on)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn7, runFn2, runFn3, runFn4, runFn7)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A type that represents all varieties of JSON value.
-- |
-- | This is not a PureScript sum type, instead the underlying JSON
-- | representation is used for efficiency and performance reasons.
foreign import data Json :: Type

instance eqJson :: Eq Json where
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

instance ordJson :: Ord Json where
  compare a b =
    runFn7 _case
      (\x -> runFn7 _case (compare x) _gt _gt _gt _gt _gt a)
      (\x -> runFn7 _case _lt (compare x) _gt _gt _gt _gt a)
      (\x -> runFn7 _case _lt _lt (compare x) _gt _gt _gt a)
      (\x -> runFn7 _case _lt _lt _lt (compare x) _gt _gt a)
      (\x -> runFn7 _case _lt _lt _lt _lt (compare x) _gt a)
      (\x -> runFn7 _case _lt _lt _lt _lt _lt (compare x) a)
      b

_lt :: forall a. a -> Ordering
_lt _ = LT

_gt :: forall a. a -> Ordering
_gt _ = GT

-- | A type that represents JSON objects.
foreign import data Object :: Type

instance eqObject :: Eq Object where
  eq = eq `on` entries

instance ordObject :: Ord Object where
  compare = compare `on` entries

-- | A type that represents the JSON `null` value.
foreign import data Null :: Type

instance eqNull :: Eq Null where
  eq _ _ = true

instance ordNull :: Ord Null where
  compare _ _ = EQ

-- | Attempts to parse a string as a JSON value. If parsing fails, an error
-- | message detailing the cause may be returned in the `Left` of the result.
parse :: String -> Either String Json
parse j = runFn3 _parse Left Right j

foreign import _parse
  :: Fn3
      (forall a b. a -> Either a b)
      (forall a b. b -> Either a b)
      String
      (Either String Json)

-- | Prints a JSON value as a compact (single line) string.
foreign import print :: Json -> String

-- | Prints a JSON value as a "pretty" string, using the specified number of
-- | spaces for indentation.
foreign import printIndented :: Int -> Json -> String

-- | Performs case analysis on a JSON value.
-- |
-- | As the `Json` type is not a PureScript sum type, pattern matching cannot
-- | be used to discriminate between the potential varieties of value. This
-- | function provides an equivalent mechanism by accepting functions that deal
-- | with each variety, similar to an exaustive `case` statement.
case_
  :: forall a
   . (Null -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array Json -> a)
  -> (Object -> a)
  -> Json
  -> a
case_ a b c d e f json = runFn7 _case a b c d e f json

foreign import _case
  :: forall a
   . Fn7
      (Null -> a)
      (Boolean -> a)
      (Number -> a)
      (String -> a)
      (Array Json -> a)
      (Object -> a)
      Json
      a

fail :: forall a b. a -> Maybe b
fail _ = Nothing

-- | Converts a `Json` value to `Null` if the `Json` is `null`.
toNull :: Json -> Maybe Null
toNull json = runFn7 _case Just fail fail fail fail fail json

-- | Converts a `Json` value to `Boolean` if the `Json` is a boolean.
toBoolean :: Json -> Maybe Boolean
toBoolean json = runFn7 _case fail Just fail fail fail fail json

-- | Converts a `Json` value to `Number` if the `Json` is a number.
toNumber :: Json -> Maybe Number
toNumber json = runFn7 _case fail fail Just fail fail fail json

-- | Converts a `Json` value to `String` if the `Json` is a string.
toString :: Json -> Maybe String
toString json = runFn7 _case fail fail fail Just fail fail json

-- | Converts a `Json` value to `Array Json` if the `Json` is an array.
toArray :: Json -> Maybe (Array Json)
toArray json = runFn7 _case fail fail fail fail Just fail json

-- | Converts a `Json` value to `Object` if the `Json` is an object.
toObject :: Json -> Maybe Object
toObject json = runFn7 _case fail fail fail fail fail Just json

-- | The JSON `null` value.
null :: Null
null = _null

foreign import _null :: Null

-- | Creates a `Json` value from a `Boolean`.
foreign import fromBoolean :: Boolean -> Json

-- | Creates a `Json` value from a `Number`.
-- |
-- | The PureScript `Number` type admits infinities and a `NaN` value which are
-- | not allowed in JSON, so when encountered, this function will treat those
-- | values as 0.
fromNumber ∷ Number -> Json
fromNumber = fromNumberWithDefault 0

-- | Creates a `Json` value from a `Number`, using a fallback `Int` value for
-- | cases where the PureScript number value is not valid for JSON.
foreign import fromNumberWithDefault :: Int -> Number -> Json

-- | Creates a `Json` value from an `Int`.
-- |
-- | There is no corresponding `toInt` as JSON doesn't have a concept of
-- | integers - this is provided as a convenience to avoid having to convert
-- | `Int` to `Number` before creating a `Json` value.
foreign import fromInt :: Int -> Json

-- | Creates a `Json` value from a `String`.
-- |
-- | **Note**: this does not parse a string as a JSON value, it takes a
-- | PureScript string and produces the corresponding `Json` value for that
-- | string, similar to the other functions like `fromBoolean` and `fromNumber`.
-- |
-- | To take a string that contains printed JSON and turn it into a `Json`
-- | value, see [`parse`](#v:parse).
foreign import fromString :: String -> Json

-- | Creates a `Json` value from an array of `Json` values.
foreign import fromArray :: Array Json -> Json

-- | Creates a `Json` value from an `Object`.
foreign import fromObject :: Object -> Json

-- | Extracts the key/value pairs of an `Object`.
entries :: Object -> Array (Tuple String Json)
entries obj = runFn2 _entries Tuple obj

foreign import _entries :: forall c. Fn2 (String -> Json -> c) Object (Array c)

-- | Extracts the keys of an `Object`.
keys :: Object -> Array String
keys obj = runFn2 _entries (\k _ → k) obj

-- | Extracts the values of an `Object`.
values :: Object -> Array Json
values obj = runFn2 _entries (\_ v → v) obj

-- | Attempts to fetch the value for a key from an `Object`. If the key is not
-- | present `Nothing` is returned.
lookup :: String -> Object -> Maybe Json
lookup k obj = runFn4 _lookup Nothing Just k obj

foreign import _lookup
  :: Fn4
      (forall a. Maybe a)
      (forall a. a -> Maybe a)
      String
      Object
      (Maybe Json)
