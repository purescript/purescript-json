module JSON
  ( parse
  , null
  , fromBoolean
  , fromNumberWithDefault
  , fromNumber
  , fromInt
  , fromString
  , fromArray
  , fromObject
  , case_
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , print
  , printIndented
  , module Internal
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, runFn3, runFn7)
import Data.Maybe (Maybe(..))
import JSON.Internal (JSON) as Internal
import JSON.Internal (JSON, Object, _case, _fromNumberWithDefault, _parse)

-- | Attempts to parse a string as a JSON value. If parsing fails, an error message detailing the
-- | cause may be returned in the `Left` of the result.
parse :: String -> Either String JSON
parse j = runFn3 _parse Left Right j

-- | The JSON `null` value.
null :: JSON
null = _null

foreign import _null :: JSON

-- | Creates a `JSON` value from a `Boolean`.
foreign import fromBoolean :: Boolean -> JSON

-- | Creates a `JSON` value from a `Number`, using a fallback `Int` value for cases where the
-- | PureScript number value is not valid for JSON.
fromNumberWithDefault :: Int -> Number -> JSON
fromNumberWithDefault fallback n = runFn2 _fromNumberWithDefault fallback n

-- | Creates a `JSON` value from a `Number`.
-- |
-- | The PureScript `Number` type admits infinities and a `NaN` value which are not allowed in JSON,
-- | so when encountered, this function will treat those values as 0.
fromNumber :: Number -> JSON
fromNumber n = runFn2 _fromNumberWithDefault 0 n

-- | Creates a `JSON` value from an `Int`.
-- |
-- | There is no corresponding `toInt` as JSON doesn't have a concept of integers - this is provided
-- | as a convenience to avoid having to convert `Int` to `Number` before creating a `JSON` value.
foreign import fromInt :: Int -> JSON

-- | Creates a `JSON` value from a `String`.
-- |
-- | **Note**: this does not parse a string as a JSON value, it takes a PureScript string and
-- | produces the corresponding `JSON` value for that string, similar to the other functions like
-- | `fromBoolean` and `fromNumber`.
-- |
-- | To take a string that contains printed JSON and turn it into a `JSON` value, see
-- | [`parse`](#v:parse).
foreign import fromString :: String -> JSON

-- | Creates a `JSON` value from an array of `JSON` values.
foreign import fromArray :: Array JSON -> JSON

-- | Creates a `JSON` value from an `Object`.
foreign import fromObject :: Object -> JSON

-- | Performs case analysis on a JSON value.
-- |
-- | As the `JSON` type is not a PureScript sum type, pattern matching cannot be used to
-- | discriminate between the potential varieties of value. This function provides an equivalent
-- | mechanism by accepting functions that deal with each variety, similar to an exaustive `case`
-- | statement.
case_
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array JSON -> a)
  -> (Object -> a)
  -> JSON
  -> a
case_ a b c d e f json = runFn7 _case a b c d e f json

fail :: forall a b. a -> Maybe b
fail _ = Nothing

-- | Converts a `JSON` value to `Null` if the `JSON` is `null`.
toNull :: JSON -> Maybe Unit
toNull json = runFn7 _case Just fail fail fail fail fail json

-- | Converts a `JSON` value to `Boolean` if the `JSON` is a boolean.
toBoolean :: JSON -> Maybe Boolean
toBoolean json = runFn7 _case fail Just fail fail fail fail json

-- | Converts a `JSON` value to `Number` if the `JSON` is a number.
toNumber :: JSON -> Maybe Number
toNumber json = runFn7 _case fail fail Just fail fail fail json

-- | Converts a `JSON` value to `String` if the `JSON` is a string.
toString :: JSON -> Maybe String
toString json = runFn7 _case fail fail fail Just fail fail json

-- | Converts a `JSON` value to `Array JSON` if the `JSON` is an array.
toArray :: JSON -> Maybe (Array JSON)
toArray json = runFn7 _case fail fail fail fail Just fail json

-- | Converts a `JSON` value to `Object` if the `JSON` is an object.
toObject :: JSON -> Maybe Object
toObject json = runFn7 _case fail fail fail fail fail Just json

-- | Prints a JSON value as a compact (single line) string.
foreign import print :: JSON -> String

-- | Prints a JSON value as a "pretty" string,
foreign import printIndented :: JSON -> String
