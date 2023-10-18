module JSON.Array
  ( fromFoldable
  , singleton
  , index
  , toUnfoldable
  , module Exports
  ) where

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (runFn4)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (class Unfoldable)
import JSON.Internal (JArray, JSON, _index, fromArray, toArray)
import JSON.Internal (JArray, empty, fromArray, length, toArray) as Exports

-- | Creates a `JArray` from a `Foldable` source of `JSON`.
fromFoldable :: forall f. Foldable f => f JSON -> JArray
fromFoldable js = fromArray (Array.fromFoldable js)

-- | Creates a `JArray` with a single entry.
foreign import singleton :: JSON -> JArray

-- | Attempts to read a value from the specified index of a `JArray`.
index :: Int -> JArray -> Maybe JSON
index ix arr = runFn4 _index Nothing Just ix arr

-- | Unfolds a `JArray` into `JSON` items
toUnfoldable :: forall f. Unfoldable f => JArray -> f JSON
toUnfoldable js = Array.toUnfoldable (toArray js)
