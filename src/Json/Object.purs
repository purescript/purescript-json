module Json.Object
  ( fromFoldable
  , fromFoldableWithIndex
  , module Exports
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Tuple (Tuple(..))
import Json.Internal (Json, Object)
import Json.Internal (Object, keys, values, entries, lookup) as Exports
import Json.Object.ST as ST

fromFoldable :: forall f. Foldable f => f (Tuple String Json) -> Object
fromFoldable kvs =
  ST.run (foldl (\acc (Tuple k v) -> ST.poke k v =<< acc) ST.new kvs)

fromFoldableWithIndex :: forall f. FoldableWithIndex String f => f Json -> Object
fromFoldableWithIndex kvs =
  ST.run (foldlWithIndex (\k acc v -> ST.poke k v =<< acc) ST.new kvs)
