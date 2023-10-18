module JSON
  ( parse
  , null
  , fromBoolean
  , fromNumber
  , fromNumberWithDefault
  , fromInt
  , fromString
  , fromArray
  , fromJArray
  , fromJObject
  , case_
  , toNull
  , toBoolean
  , toNumber
  , toInt
  , toString
  , toArray
  , toJArray
  , toJObject
  , print
  , printIndented
  , module Exports
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, runFn3, runFn7)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import JSON.Internal (JArray, JObject, JSON)
import JSON.Internal (JArray, JObject, JSON) as Exports
import JSON.Internal as Internal

-- | Attempts to parse a string as a JSON value. If parsing fails, an error message detailing the
-- | cause may be returned in the `Left` of the result.
parse :: String -> Either String JSON
parse j = runFn3 Internal._parse Left Right j

-- | The JSON `null` value.
null :: JSON
null = _null

-- | The JSON `null` value.
foreign import _null :: JSON

-- | Converts a `Boolean` into `JSON`.
foreign import fromBoolean :: Boolean -> JSON

-- | Converts a `Number` into `JSON`.
-- |
-- | The PureScript `Number` type admits infinities and a `NaN` value which are not allowed in JSON,
-- | so when encountered, this function will treat those values as 0.
fromNumber :: Number -> JSON
fromNumber n = runFn2 Internal._fromNumberWithDefault 0 n

-- | Creates a `Number` into `JSON`, using a fallback `Int` value for cases where the
-- | PureScript number value is not valid for JSON (`NaN`, `infinity`).
fromNumberWithDefault :: Int -> Number -> JSON
fromNumberWithDefault fallback n = runFn2 Internal._fromNumberWithDefault fallback n

-- | Converts an `Int` into `JSON`.
-- |
-- | Note: JSON doesn't have a concept of integers. This is provided
-- | as a convenience to avoid having to convert `Int` to `Number` before creating a `JSON` value.
foreign import fromInt :: Int -> JSON

-- | Converts a `String` into `JSON`.
-- |
-- | **Note**: this does not parse a string as a JSON value, it takes a PureScript string and
-- | produces the corresponding `JSON` value for that string, similar to the other functions like
-- | `fromBoolean` and `fromNumber`.
-- |
-- | To take a string that contains printed JSON and turn it into a `JSON` value, see
-- | [`parse`](#v:parse).
foreign import fromString :: String -> JSON

-- | Converts a `JArray` into `JSON`.
foreign import fromJArray :: JArray -> JSON

-- | Converts an array of `JSON` values into `JSON`.
fromArray :: Array JSON -> JSON
fromArray js = fromJArray (Internal.fromArray js)

-- | Converts a `JObject` into `JSON`.
foreign import fromJObject :: JObject -> JSON

-- | Performs case analysis on a JSON value.
-- |
-- | As the `JSON` type is not a PureScript sum type, pattern matching cannot be used to
-- | discriminate between the potential varieties of value. This function provides an equivalent
-- | mechanism by accepting functions that deal with each variety, similar to an exaustive `case`
-- | statement.
-- |
-- | The `Unit` case is for `null` values.
case_
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (JArray -> a)
  -> (JObject -> a)
  -> JSON
  -> a
case_ a b c d e f json = runFn7 Internal._case a b c d e f json

fail :: forall a b. a -> Maybe b
fail _ = Nothing

-- | Converts a `JSON` value to `Null` if the `JSON` is `null`.
toNull :: JSON -> Maybe Unit
toNull json = runFn7 Internal._case Just fail fail fail fail fail json

-- | Converts a `JSON` value to `Boolean` if the `JSON` is a boolean.
toBoolean :: JSON -> Maybe Boolean
toBoolean json = runFn7 Internal._case fail Just fail fail fail fail json

-- | Converts a `JSON` value to `Number` if the `JSON` is a number.
toNumber :: JSON -> Maybe Number
toNumber json = runFn7 Internal._case fail fail Just fail fail fail json

-- | Converts a `JSON` `Number` into an `Int`.
-- |
-- | This is provided for convenience only.
toInt :: JSON -> Maybe Int
toInt = toNumber >=> Int.fromNumber

-- | Converts a `JSON` value to `String` if the `JSON` is a string.
toString :: JSON -> Maybe String
toString json = runFn7 Internal._case fail fail fail Just fail fail json

-- | Converts a `JSON` value to `JArray` if the `JSON` is an array.
toJArray :: JSON -> Maybe JArray
toJArray json = runFn7 Internal._case fail fail fail fail Just fail json

-- | Converts a `JSON` value to `Array JSON` if the `JSON` is an array.
toArray :: JSON -> Maybe (Array JSON)
toArray json = Internal.toArray <$> toJArray json

-- | Converts a `JSON` value to `Object` if the `JSON` is an object.
toJObject :: JSON -> Maybe JObject
toJObject json = runFn7 Internal._case fail fail fail fail fail Just json

-- | Prints a JSON value as a compact (single line) string.
foreign import print :: JSON -> String

-- | Prints a JSON value as a "pretty" string with newlines and indentation.
foreign import printIndented :: JSON -> String
