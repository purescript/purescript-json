module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import JSON as J
import JSON.Array as JA
import JSON.Object as JO
import JSON.Path as Path
import Test.Assert (assertEqual, assertTrue)

main :: Effect Unit
main = do

  log "Check numeric comparisons"
  assertTrue $ J.fromInt 1 == J.fromInt 1
  assertTrue $ J.fromInt 1 < J.fromInt 2
  assertTrue $ J.fromInt 42 > J.fromInt 0

  log "Check string comparisons"
  assertTrue $ J.fromString "json" == J.fromString "json"
  assertTrue $ J.fromString "a" < J.fromString "b"
  assertTrue $ J.fromString "q" > J.fromString "p"

  log "Check array comparisons"
  assertTrue $ J.fromJArray (JA.fromArray []) == J.fromJArray (JA.fromArray [])
  assertTrue $ J.fromJArray (JA.fromArray [ J.fromInt 1 ]) == J.fromJArray (JA.fromArray [ J.fromInt 1 ])
  assertTrue $ J.fromJArray (JA.fromArray [ J.fromInt 1 ]) < J.fromJArray (JA.fromArray [ J.fromInt 2 ])

  log "Check object comparisons"
  assertTrue $ JO.empty == JO.empty
  assertTrue $ J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1) ]) == J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1) ])
  assertTrue $ J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1) ]) < J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 2) ])

  log "Check array index"
  assertTrue $ JA.index (-1) (JA.fromArray (J.fromInt <$> [ 0, 2, 4 ])) == Nothing
  assertTrue $ JA.index 0 (JA.fromArray (J.fromInt <$> [ 0, 2, 4 ])) == Just (J.fromInt 0)
  assertTrue $ JA.index 1 (JA.fromArray (J.fromInt <$> [ 0, 2, 4 ])) == Just (J.fromInt 2)
  assertTrue $ JA.index 2 (JA.fromArray (J.fromInt <$> [ 0, 2, 4 ])) == Just (J.fromInt 4)
  assertTrue $ JA.index 3 (JA.fromArray (J.fromInt <$> [ 0, 2, 4 ])) == Nothing

  log "Check array concat"
  assertTrue $ JA.fromArray (J.fromInt <$> [ 1, 2 ]) <> JA.fromArray (J.fromInt <$> [ 2, 3 ]) == JA.fromArray (J.fromInt <$> [ 1, 2, 2, 3 ])

  log "Check path printing"
  assertEqual
    { expected: "$.data[0].field"
    , actual: Path.print (Path.AtKey "field" (Path.AtIndex 0 (Path.AtKey "data" Path.Top)))
    }

  log "Check path get"
  assertTrue $ Path.get Path.Top (J.fromString "hello") == Just (J.fromString "hello")
  assertTrue $ Path.get Path.Top (J.fromJArray (JA.fromArray [ J.fromInt 42 ])) == Just (J.fromJArray (JA.fromArray [ J.fromInt 42 ]))
  assertTrue $ Path.get (Path.AtIndex 0 Path.Top) (J.fromJArray (JA.fromArray [ J.fromInt 42, J.fromString "X", J.fromBoolean true ])) == Just (J.fromInt 42)
  assertTrue $ Path.get (Path.AtIndex 1 Path.Top) (J.fromJArray (JA.fromArray [ J.fromInt 42, J.fromString "X", J.fromBoolean true ])) == Just (J.fromString "X")
  assertTrue $ Path.get (Path.AtIndex 5 Path.Top) (J.fromJArray (JA.fromArray [ J.fromInt 42, J.fromString "X", J.fromBoolean true ])) == Nothing
  assertTrue $ Path.get (Path.AtKey "a" Path.Top) (J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1), Tuple "x" (J.fromBoolean false) ])) == Just (J.fromInt 1)
  assertTrue $ Path.get (Path.AtKey "x" Path.Top) (J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1), Tuple "x" (J.fromBoolean false) ])) == Just (J.fromBoolean false)
  assertTrue $ Path.get (Path.AtKey "z" Path.Top) (J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1), Tuple "x" (J.fromBoolean false) ])) == Nothing
  assertTrue $ Path.get (Path.AtKey "x" (Path.AtIndex 1 Path.Top)) (J.fromJArray (JA.fromArray [ J.fromString "skip", (J.fromJObject (JO.fromEntries [ Tuple "a" (J.fromInt 1), Tuple "x" (J.fromBoolean false) ])) ])) == Just (J.fromBoolean false)
