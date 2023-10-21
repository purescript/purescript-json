module JSON.Path where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as JSON
import JSON.Array as JArray
import JSON.Object as JObject

-- | A path to a location in a JSON document.
data Path
  = Tip
  | AtKey String Path
  | AtIndex Int Path

derive instance Eq Path
derive instance Ord Path
derive instance Generic Path _

instance Show Path where
  show = case _ of
    Tip -> "Tip"
    AtKey key rest -> "(AtKey " <> show key <> " " <> show rest <> ")"
    AtIndex ix rest -> "(AtIndex " <> show ix <> " " <> show rest <> ")"

-- | Attempts to get the value at the path in a JSON document.
get :: Path -> JSON -> Maybe JSON
get path json =
  case path of
    Tip -> Just json
    AtKey key rest -> get rest =<< JObject.lookup key =<< JSON.toJObject json
    AtIndex ix rest -> get rest =<< JArray.index ix =<< JSON.toJArray json

-- | Prints the path as a basic JSONPath expression.
print :: Path -> String
print path = "$" <> go path
  where
  go :: Path -> String
  go p = case p of
    Tip -> ""
    AtKey k rest -> "." <> k <> go rest -- TODO: ["quoted"] paths also
    AtIndex ix rest -> "[" <> show ix <> "]" <> go rest

-- | Extends the tip of the first path with the second path.
-- |
-- | For example, `$.data[0]` extended with `$.info.title` would result in `$.data[0].info.title`.
extend :: Path -> Path -> Path
extend p1 p2 = case p1 of
  Tip -> p2
  AtKey key rest -> AtKey key (extend rest p2)
  AtIndex ix rest -> AtIndex ix (extend rest p2)

-- | Finds the common prefix of two paths. If they have nothing in common the result will be the
-- | root.
findCommonPrefix :: Path -> Path -> Path
findCommonPrefix = case _, _ of
  AtKey k1 rest1, AtKey k2 rest2 | k1 == k2 -> AtKey k1 (findCommonPrefix rest1 rest2)
  AtIndex i1 rest1, AtIndex i2 rest2 | i1 == i2 -> AtIndex i1 (findCommonPrefix rest1 rest2)
  _, _ -> Tip

-- | Attempts to strip the first path from the start of the second path. `Nothing` is returned if
-- | the second path does not start with the prefix.
-- |
-- | For example, stripping a prefix of `$.data[0]` from `$.data[0].info.title` would result in
-- | `$.info.title`.
stripPrefix :: Path -> Path -> Maybe Path
stripPrefix = case _, _ of
  AtKey k1 rest1, AtKey k2 rest2 | k1 == k2 -> stripPrefix rest1 rest2
  AtIndex i1 rest1, AtIndex i2 rest2 | i1 == i2 -> stripPrefix rest1 rest2
  Tip, tail -> Just tail
  _, _ -> Nothing
