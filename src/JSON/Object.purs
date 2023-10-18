module JSON.Object
  ( fromEntries
  , fromFoldable
  , fromFoldableWithIndex
  , empty
  , singleton
  , insert
  , delete
  , entries
  , keys
  , values
  , lookup
  , toUnfoldable
  , module Exports
  ) where

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)
import Data.Function.Uncurried (runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import JSON.Internal (JObject) as Exports
import JSON.Internal (JObject, JSON, _delete, _entries, _fromEntries, _insert, _lookup)

-- | Creates an `JObject` from an array of key/value pairs.
fromEntries :: Array (Tuple String JSON) -> JObject
fromEntries kvs = runFn3 _fromEntries fst snd kvs

-- | Creates an `JObject` from a foldable source of key/value pairs.
fromFoldable :: forall f. Foldable f => f (Tuple String JSON) -> JObject
fromFoldable kvs = fromEntries (Array.fromFoldable kvs)

-- | Creates an `JObject` from an indexed foldable source.
fromFoldableWithIndex :: forall f. FoldableWithIndex String f => f JSON -> JObject
fromFoldableWithIndex kvs = fromEntries (foldrWithIndex (\k v -> Array.cons (Tuple k v)) [] kvs)

-- | An empty `JObject`.
foreign import empty :: JObject

-- | Creates an `JObject` with a single entry.
singleton :: String -> JSON -> JObject
singleton k v = runFn3 _insert k v empty

-- | Inserts an entry into an `JObject`. If the key already exists the value will be overwritten.
insert :: String -> JSON -> JObject -> JObject
insert k v obj = runFn3 _insert k v obj

-- | Deletes an entry from an `JObject`. This will have no effect if the key does not exist in the
-- | object.
delete :: String -> JObject -> JObject
delete k obj = runFn2 _delete k obj

-- | Extracts the key/value pairs of an `JObject`.
entries :: JObject -> Array (Tuple String JSON)
entries obj = runFn2 _entries Tuple obj

-- | Extracts the keys of an `JObject`.
keys :: JObject -> Array String
keys obj = runFn2 _entries (\k _ -> k) obj

-- | Extracts the values of an `JObject`.
values :: JObject -> Array JSON
values obj = runFn2 _entries (\_ v -> v) obj

-- | Attempts to fetch the value for a key from an `JObject`. If the key is not present `Nothing` is
-- | returned.
lookup :: String -> JObject -> Maybe JSON
lookup k obj = runFn4 _lookup Nothing Just k obj

-- | Unfolds an object into key/value pairs.
toUnfoldable :: forall f. Unfoldable f => JObject -> f (Tuple String JSON)
toUnfoldable obj = Array.toUnfoldable (entries obj)
