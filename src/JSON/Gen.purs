module JSON.Gen where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Data.String.Gen (genUnicodeString)
import Data.Tuple (Tuple(..))
import JSON as J
import JSON.Array as JArray
import JSON.Object as JObject

-- | A generator for random `JSON` values of any variety.
genJSON :: forall m. MonadGen m => MonadRec m => Lazy (m J.JSON) => m J.JSON
genJSON = Gen.resize (min 5) $ Gen.sized genJSON'
  where
  genJSON' :: Int -> m J.JSON
  genJSON' size
    | size > 1 = Gen.resize (_ - 1) (Gen.choose genArray genObject)
    | otherwise = genLeaf

-- | A generator for JSON arrays containing items based on the passed generator.
genJArrayOf :: forall m. MonadGen m => MonadRec m => m J.JSON -> m J.JArray
genJArrayOf inner = JArray.fromArray <$> Gen.unfoldable inner

-- | A generator for JSON arrays containing random items.
genJArray :: forall m. MonadGen m => MonadRec m => Lazy (m J.JSON) => m J.JArray
genJArray = genJArrayOf (defer \_ -> genJSON)

-- | A generator for JSON vaues that are arrays containing items based on the passed generator.
genArrayOf :: forall m. MonadGen m => MonadRec m => m J.JSON -> m J.JSON
genArrayOf inner = J.fromJArray <$> genJArrayOf inner

-- | A generator for JSON vaues that are arrays containing random items.
genArray :: forall m. MonadGen m => MonadRec m => Lazy (m J.JSON) => m J.JSON
genArray = J.fromJArray <$> genJArray

-- | A generator for JSON objects containing entries based on the passed generator.
genJObjectOf :: forall m. MonadGen m => MonadRec m => m (Tuple String J.JSON) -> m J.JObject
genJObjectOf inner = JObject.fromEntries <$> (Gen.unfoldable inner)

-- | A generator for JSON objects containing random entries.
genJObject :: forall m. MonadGen m => MonadRec m => Lazy (m J.JSON) => m J.JObject
genJObject = genJObjectOf (Tuple <$> genUnicodeString <*> defer \_ -> genJSON)

-- | A generator for JSON values that are objects containing entries based on the passed generator.
genObjectOf :: forall m. MonadGen m => MonadRec m => m (Tuple String J.JSON) -> m J.JSON
genObjectOf inner = J.fromJObject <$> genJObjectOf inner

-- | A generator for JSON values that are objects containing random entries.
genObject :: forall m. MonadGen m => MonadRec m => Lazy (m J.JSON) => m J.JSON
genObject = J.fromJObject <$> genJObject

-- | A generator for JSON leaf (null, boolean, number, string) values.
genLeaf :: forall m. MonadGen m => MonadRec m => m J.JSON
genLeaf = Gen.oneOf $ pure J.null :| [ genBoolean, genNumber, genString ]

-- | A generator for JSON booleans.
genBoolean :: forall m. MonadGen m => m J.JSON
genBoolean = J.fromBoolean <$> Gen.chooseBool

-- | A generator for JSON numbers.
genNumber :: forall m. MonadGen m => m J.JSON
genNumber = J.fromNumber <$> Gen.chooseFloat (-1000000.0) 1000000.0

-- | A generator for JSON integers.
genInt :: forall m. MonadGen m => m J.JSON
genInt = J.fromInt <$> Gen.chooseInt (-1000000) 1000000

-- | A generator for JSON strings.
genString :: forall m. MonadGen m => MonadRec m => m J.JSON
genString = J.fromString <$> genUnicodeString
