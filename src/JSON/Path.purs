module JSON.Path where

import Prelude

import Data.Maybe (Maybe(..))
import JSON (JSON)
import JSON as JSON
import JSON.Array as JArray
import JSON.Object as JObject

data Path
  = Top
  | AtKey String Path
  | AtIndex Int Path

derive instance Eq Path
derive instance Ord Path

instance Show Path where
  show = case _ of
    Top -> "Top"
    AtKey key rest -> "(AtKey " <> show key <> " " <> show rest <> ")"
    AtIndex ix rest -> "(AtIndex " <> show ix <> " " <> show rest <> ")"

get :: Path -> JSON -> Maybe JSON
get path json =
  case path of
    Top -> Just json
    AtKey key rest -> JObject.lookup key =<< JSON.toJObject =<< get rest json
    AtIndex ix rest -> JArray.index ix =<< JSON.toJArray =<< get rest json

print :: Path -> String
print path = "$" <> go path ""
  where
  go :: Path -> String -> String
  go p acc = case p of
    Top -> acc
    AtKey k rest -> go rest ("." <> k <> acc)
    AtIndex ix rest -> go rest ("[" <> show ix <> "]" <> acc)
