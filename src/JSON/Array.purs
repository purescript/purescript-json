module JSON.Array
  ( fromFoldable
  , empty
  , singleton
  , index
  , toUnfoldable
  , module Exports
  ) where

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Unfoldable (class Unfoldable)
import JSON.Internal (JArray, JSON, toArray, fromArray)
import JSON.Internal (JArray, toArray, fromArray) as Exports

-- | Creates a `JArray` from a `Foldable` source of `JSON`.
fromFoldable :: forall f. Foldable f => f JSON -> JArray
fromFoldable js = fromArray (Array.fromFoldable js)

-- | An empty `JArray`.
empty :: JArray
empty = fromArray []

-- | Creates a `JArray` with a single entry.
singleton :: JSON -> JArray
singleton j = fromArray [ j ]

-- | Attempts to read a value from the specified index of a `JArray`.
index :: JArray -> Int -> Maybe JSON
index js = Array.index (toArray js)

-- | Unfolds a `JArray` into `JSON` items
toUnfoldable :: forall f. Unfoldable f => JArray -> f JSON
toUnfoldable js = Array.toUnfoldable (toArray js)
