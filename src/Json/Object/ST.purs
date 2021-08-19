module Json.Object.ST
  ( STObject
  , new
  , poke
  , thaw
  , freeze
  , run
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Data.Function.Uncurried (Fn3, runFn3)
import Json.Internal (Json, Object)

foreign import data STObject :: Region -> Type

foreign import new :: forall r. ST r (STObject r)

poke :: forall r. String -> Json -> STObject r -> ST r Unit
poke key value obj = runFn3 _poke key value obj

foreign import _poke :: forall r. Fn3 String Json (STObject r) (ST r Unit)

foreign import thaw :: forall r. Object -> ST r (STObject r)

foreign import freeze :: forall r. STObject r -> ST r (Object)

foreign import run :: (forall r. ST r (STObject r)) -> Object
