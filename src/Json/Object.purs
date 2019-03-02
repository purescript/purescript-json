module Json.Object where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Json (Json, Object)
import Json.Object.ST as ST

foreign import keys :: Object -> Array String

foreign import values :: Object -> Array Json

foreign import _key :: Fn4 (forall a. Maybe a) (forall a. a -> Maybe a) String Object (Maybe Json)

key :: String -> Object -> Maybe Json
key k obj = runFn4 _key Nothing Just k obj

fromFoldable :: forall f. Foldable f => f (Tuple String Json) -> Object
fromFoldable kvs =
  ST.run (foldl (\acc (Tuple k v) -> ST.poke k v =<< acc) ST.new kvs)

fromFoldableWithIndex :: forall f. FoldableWithIndex String f => f Json -> Object
fromFoldableWithIndex kvs =
  ST.run (foldlWithIndex (\k acc v -> ST.poke k v =<< acc) ST.new kvs)
