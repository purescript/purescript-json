module Json.Object.ST
  ( STObject
  , new
  , peek
  , poke
  , delete
  , thaw
  , freeze
  , run
  ) where

import Control.Monad.ST (ST, kind Region)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Json (Json, Object)

foreign import data STObject :: Region -> Type

foreign import _new :: forall r. ST r (STObject r)

new :: forall r. ST r (STObject r)
new = _new

foreign import _peek :: forall b r. Fn4 (forall a. a -> Maybe a) (forall a. Maybe a) String (STObject r) (ST r b)

peek :: forall r. String -> STObject r -> ST r (Maybe Json)
peek key obj = runFn4 _peek Just Nothing key obj

foreign import _poke :: forall r. Fn3 String Json (STObject r) (ST r (STObject r))

poke :: forall r. String -> Json -> STObject r -> ST r (STObject r)
poke key value obj = runFn3 _poke key value obj

foreign import _delete :: forall r. Fn2 String (STObject r) (ST r (STObject r))

delete :: forall r. String -> STObject r -> ST r (STObject r)
delete key obj = runFn2 _delete key obj

foreign import _copyST :: forall a b r. a -> ST r b

thaw :: forall r. Object -> ST r (STObject r)
thaw = _copyST

freeze :: forall r. STObject r -> ST r (Object)
freeze = _copyST

foreign import run :: (forall r. ST r (STObject r)) -> Object
