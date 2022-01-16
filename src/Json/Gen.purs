module Json.Gen where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))
import Json as J
import Json.Object as Object

-- | A generator for `Json` values. Especially useful for writing property-based tests.
genJson :: forall m. MonadGen m => MonadRec m => Lazy (m J.Json) => m J.Json
genJson = Gen.resize (min 5) $ Gen.sized genJson'
  where
  genJson' :: Int -> m J.Json
  genJson' size
    | size > 1 = Gen.resize (_ - 1) (Gen.choose genArray genObject)
    | otherwise = genLeaf

  genArray :: m J.Json
  genArray = J.fromArray <$> Gen.unfoldable (defer \_ -> genJson)

  genObject :: m J.Json
  genObject = J.fromObject <<< Object.fromFoldable <$> genObjectEntries

  genObjectEntries :: m (Array (Tuple String J.Json))
  genObjectEntries = Gen.unfoldable (Tuple <$> genUnicodeString <*> (defer \_ -> genJson))

  genLeaf :: m J.Json
  genLeaf = Gen.oneOf $ pure J.null :| [ genBoolean, genNumber, genString ]

  genBoolean :: m J.Json
  genBoolean = J.fromBoolean <$> Gen.chooseBool

  genNumber :: m J.Json
  genNumber = J.fromNumber <$> Gen.chooseFloat (-1000000.0) 1000000.0

  genString :: m J.Json
  genString = J.fromString <$> genUnicodeString
