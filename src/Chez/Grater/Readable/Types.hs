-- |Types and instances for instances for humans to read.
module Chez.Grater.Readable.Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.CI.Orphans ()
import Chez.Grater.Internal.Json (jsonOptions)
import Chez.Grater.Types (Ingredient(..), IngredientName(..), Quantity(..), Unit(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
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

deriveJSON (jsonOptions "readableFraction") ''ReadableFraction
deriveJSON (jsonOptions "readableQuantity") ''ReadableQuantity
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient

mkReadableQuantity :: Quantity -> ReadableQuantity
mkReadableQuantity q = case splitQuantity q of
  Nothing -> ReadableQuantity Nothing Nothing
  Just (w, d) ->
    case (w == 0, find (\((lo, hi), _) -> lo <= d && d <= hi) knownQuantities) of
      (False, Just (_, (numerator, denominator))) -> ReadableQuantity (Just w) (Just (ReadableFraction numerator denominator))
      (True, Just (_, (numerator, denominator))) -> ReadableQuantity Nothing (Just (ReadableFraction numerator denominator))
      (False, Nothing) -> ReadableQuantity (Just w) Nothing
      (True, Nothing) -> ReadableQuantity Nothing Nothing

  where

    quantityPrecision :: Double
    quantityPrecision = 0.01

    quarter = 0.25
    third = 1 / 3
    half = 0.5
    twoThird = 2 / 3
    threeQuarter = 0.75

    knownQuantities :: [((Double, Double), (Int, Int))]
    knownQuantities =
      [ ((quarter - quantityPrecision, quarter + quantityPrecision), (1, 4))
      , ((third - quantityPrecision, third + quantityPrecision), (1, 3))
      , ((half - quantityPrecision, half + quantityPrecision), (1, 2))
      , ((twoThird - quantityPrecision, twoThird + quantityPrecision), (2, 3))
      , ((threeQuarter - quantityPrecision, threeQuarter + quantityPrecision), (3, 4))
      ]

    splitQuantity :: Quantity -> Maybe (Int, Double)
    splitQuantity = \case
      QuantityMissing -> Nothing
      Quantity q2 ->
        case abs (fromIntegral (round q2 :: Int) - q2) < quantityPrecision of
          True -> Just (round q2, 0.0)
          False -> let w = truncate q2 in Just (w, q2 - fromIntegral w)

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