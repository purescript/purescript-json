module Json
  ( module Json.Types
  , parse
  , print
  , printPretty

  , case_
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject

  , null
  , fromBoolean
  , fromNumber
  , fromString
  , fromArray
  , fromObject
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn7, runFn3, runFn7)
import Data.Maybe (Maybe(..))
import Json.Types (Json, Object)

foreign import _parse
  :: Fn3
      (forall a b. a -> Either a b)
      (forall a b. b -> Either a b)
      String
      (Either String Json)

parse :: String -> Either String Json
parse j = runFn3 _parse Left Right j

foreign import print :: Json -> String

foreign import printPretty :: Int -> Json -> String

foreign import _case
  :: forall a
   . Fn7
      (Unit -> a)
      (Boolean -> a)
      (Number -> a)
      (String -> a)
      (Array Json -> a)
      (Object -> a)
      Json
      a

case_
  :: forall a
   . (Unit -> a)
  -> (Boolean -> a)
  -> (Number -> a)
  -> (String -> a)
  -> (Array Json -> a)
  -> (Object -> a)
  -> Json
  -> a
case_ a b c d e f json = runFn7 _case a b c d e f json

fail :: forall a b. a -> Maybe b
fail _ = Nothing

toNull :: Json -> Maybe Unit
toNull json = runFn7 _case Just fail fail fail fail fail json

toBoolean :: Json -> Maybe Boolean
toBoolean json = runFn7 _case fail Just fail fail fail fail json

toNumber :: Json -> Maybe Number
toNumber json = runFn7 _case fail fail Just fail fail fail json

toString :: Json -> Maybe String
toString json = runFn7 _case fail fail fail Just fail fail json

toArray :: Json -> Maybe (Array Json)
toArray json = runFn7 _case fail fail fail fail Just fail json

toObject :: Json -> Maybe Object
toObject json = runFn7 _case fail fail fail fail fail Just json

foreign import _null :: Json

null :: Json
null = _null

foreign import fromBoolean :: Boolean -> Json
foreign import fromNumber :: Number -> Json
foreign import fromString :: String -> Json
foreign import fromArray :: Array Json -> Json
foreign import fromObject :: Object -> Json
