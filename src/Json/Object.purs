module Json.Object
  ( fromFoldable
  , fromFoldableWithIndex
  , module Exports
  ) where

import Prelude

import Data.Foldable (class Foldable, for_)
import Data.FoldableWithIndex (class FoldableWithIndex, forWithIndex_)
import Data.Tuple (Tuple(..))
import Json.Internal (Json, Object)
import Json.Internal (Object, keys, values, entries, lookup) as Exports
import Json.Object.ST as ST

fromFoldable :: forall f. Foldable f => f (Tuple String Json) -> Object
fromFoldable kvs =
  ST.run do
    obj <- ST.new
    for_ kvs \(Tuple k v) -> ST.poke k v obj
    pure obj

fromFoldableWithIndex :: forall f. FoldableWithIndex String f => f Json -> Object
fromFoldableWithIndex kvs =
  ST.run do
    obj <- ST.new
    forWithIndex_ kvs \k v -> ST.poke k v obj
    pure obj
