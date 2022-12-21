module JSON.Object
  ( fromEntries
  , empty
  , singleton
  , insert
  , delete
  , entries
  , keys
  , values
  , lookup
  , fromFoldable
  , fromFoldableWithIndex
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
import JSON.Internal (JSON, Object, _delete, _entries, _fromEntries, _insert, _lookup)
import JSON.Internal (Object) as Exports

-- | An empty `Object`.
foreign import empty :: Object

-- | Creates an `Object` with a single entry.
singleton :: String -> JSON -> Object
singleton k v = runFn3 _insert k v empty

-- | Creates an `Object` from an array of key/value pairs.
fromEntries :: Array (Tuple String JSON) -> Object
fromEntries kvs = runFn3 _fromEntries fst snd kvs

-- | Creates an `Object` from a foldable source of key/value pairs.
fromFoldable :: forall f. Foldable f => f (Tuple String JSON) -> Object
fromFoldable kvs = fromEntries (Array.fromFoldable kvs)

-- | Creates an `Object` from an indexed foldable source.
fromFoldableWithIndex :: forall f. FoldableWithIndex String f => f JSON -> Object
fromFoldableWithIndex kvs = fromEntries (foldrWithIndex (\k v -> Array.cons (Tuple k v)) [] kvs)

-- | Inserts an entry into an `Object`. If the key already exists the value will be overwritten.
insert :: String -> JSON -> Object -> Object
insert k v obj = runFn3 _insert k v obj

-- | Deletes an entry from an `Object`. This will have no effect if the key does not exist in the
delete :: String -> Object -> Object
-- | object.
delete k obj = runFn2 _delete k obj

-- | Attempts to fetch the value for a key from an `Object`. If the key is not present `Nothing` is
-- | returned.
lookup :: String -> Object -> Maybe JSON
lookup k obj = runFn4 _lookup Nothing Just k obj

-- | Extracts the key/value pairs of an `Object`.
entries :: Object -> Array (Tuple String JSON)
entries obj = runFn2 _entries Tuple obj

-- | Unfolds an object into key/value pairs.
toUnfoldable :: forall f. Unfoldable f => Object -> f (Tuple String JSON)
toUnfoldable obj = Array.toUnfoldable (entries obj)

-- | Extracts the keys of an `Object`.
keys :: Object -> Array String
keys obj = runFn2 _entries (\k _ -> k) obj

-- | Extracts the values of an `Object`.
values :: Object -> Array JSON
values obj = runFn2 _entries (\_ v -> v) obj
