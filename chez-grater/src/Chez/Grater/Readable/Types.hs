-- |Types and instances for instances for humans to read.
module Chez.Grater.Readable.Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.CI.Orphans ()
import Chez.Grater.Internal.Json (jsonOptions)
import Chez.Grater.Types (Ingredient(..), IngredientName(..), Quantity(..), Step(..), Unit(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Ratio (approxRational, denominator, numerator)
import qualified Data.CaseInsensitive as CI

data ReadableFraction = ReadableFraction
  { readableFractionNumerator   :: Int
  , readableFractionDenominator :: Int
  }
  deriving (Eq, Ord, Show)

data ReadableQuantity = ReadableQuantity
  { readableQuantityWhole    :: Maybe Int
  , readableQuantityFraction :: Maybe ReadableFraction
  }
  deriving (Eq, Ord, Show)

newtype ReadableUnit = ReadableUnit { unReadableUnit :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Maybe ReadableUnit
  }
  deriving (Eq, Ord, Show)

newtype ReadableStep = ReadableStep { unReadableStep :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

deriveJSON (jsonOptions "readableFraction") ''ReadableFraction
deriveJSON (jsonOptions "readableQuantity") ''ReadableQuantity
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient

mkReadableQuantity :: Quantity -> ReadableQuantity
mkReadableQuantity = \case
  QuantityMissing -> ReadableQuantity Nothing Nothing
  Quantity q ->
    ReadableQuantity
      (if whole == 0 then Nothing else Just whole)
      (if numer == 0 then Nothing else Just (ReadableFraction numer denom))
    where
      (whole, numer) = divMod rawNumer denom

      rawNumer = fromIntegral $ numerator nearest
      denom = fromIntegral $ denominator nearest

      -- keep the denominator within reason; less than about 500:
      nearest = approxRational q 0.001

showReadableQuantity :: ReadableQuantity -> Maybe Text
showReadableQuantity ReadableQuantity {..} =
  case (readableQuantityWhole, readableQuantityFraction) of
    (Nothing, Nothing) -> Nothing
    (Just w, Nothing) -> Just $ tshow w
    (Nothing, Just ReadableFraction {..}) -> Just $ tshow readableFractionNumerator <> "/" <> tshow readableFractionDenominator
    (Just w, Just ReadableFraction {..}) -> Just $ tshow w <> " " <> tshow readableFractionNumerator <> "/" <> tshow readableFractionDenominator

mkReadableUnit :: Unit -> Maybe ReadableUnit
mkReadableUnit = \case
  Unit x -> Just (ReadableUnit x)
  UnitMissing -> Nothing

showReadableUnit :: ReadableUnit -> Text
showReadableUnit = CI.original . unReadableUnit

mkReadableIngredient :: Ingredient -> ReadableIngredient
mkReadableIngredient Ingredient {..} =
  ReadableIngredient
    { readableIngredientName = ingredientName
    , readableIngredientQuantity = mkReadableQuantity ingredientQuantity
    , readableIngredientUnit = mkReadableUnit ingredientUnit
    }

showReadableIngredient :: ReadableIngredient -> Text
showReadableIngredient ReadableIngredient {..} =
  case (showReadableQuantity readableIngredientQuantity, showReadableUnit <$> readableIngredientUnit) of
    (Nothing, Nothing) -> showIngredientName readableIngredientName
    (Just q, Nothing) -> q <> " " <> showIngredientName readableIngredientName
    (Nothing, Just u) -> u <> " " <> showIngredientName readableIngredientName
    (Just q, Just u) -> q <> " " <> u <> " " <> showIngredientName readableIngredientName
  where
    showIngredientName = CI.original . unIngredientName

mkReadableStep :: Step -> ReadableStep
mkReadableStep = ReadableStep . unStep
