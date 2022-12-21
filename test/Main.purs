module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import JSON as J
import JSON.Object as JO
import Test.Assert (assertTrue)

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
  assertTrue $ J.fromArray [] == J.fromArray []
  assertTrue $ J.fromArray [J.fromInt 1] == J.fromArray [J.fromInt 1]
  assertTrue $ J.fromArray [J.fromInt 1] < J.fromArray [J.fromInt 2]

  log "Check object comparisons"
  assertTrue $ JO.empty == JO.empty
  assertTrue $ J.fromObject (JO.fromEntries [Tuple "a" (J.fromInt 1)]) == J.fromObject (JO.fromEntries [Tuple "a" (J.fromInt 1)])
  assertTrue $ J.fromObject (JO.fromEntries [Tuple "a" (J.fromInt 1)]) < J.fromObject (JO.fromEntries [Tuple "a" (J.fromInt 2)])
