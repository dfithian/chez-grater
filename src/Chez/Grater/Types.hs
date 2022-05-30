module Chez.Grater.Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.CI.Orphans ()
import Chez.Grater.Internal.Json (jsonOptions)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)

newtype IngredientName = IngredientName { unIngredientName :: CI Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Fraction = Fraction
  { fractionNumerator   :: Int
  , fractionDenominator :: Int
  }
  deriving (Eq, Show, Ord)

data Quantity = Quantity
  { quantityWhole    :: Maybe Int
  , quantityFraction :: Maybe Fraction
  }
  deriving (Eq, Show, Ord)

newtype Unit = Unit { unUnit :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

newtype Step = Step { unStep :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Maybe Unit
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "fraction") ''Fraction
deriveJSON (jsonOptions "quantity") ''Quantity
deriveJSON (jsonOptions "Ingredient") ''Ingredient

pinch, teaspoon, tablespoon, cup, ounce, box, pound, splash, sprinkle, whole
  , milliliter, liter, milligram, gram :: Unit
pinch = Unit "pinch"
teaspoon = Unit "tsp"
tablespoon = Unit "tbsp"
cup = Unit "cup"
ounce = Unit "oz"
box = Unit "box"
pound = Unit "pound"
splash = Unit "splash"
sprinkle = Unit "sprinkle"
whole = Unit "whole"
milliliter = Unit "ml"
liter = Unit "l"
milligram = Unit "mg"
gram = Unit "g"

emptyQuantity :: Quantity
emptyQuantity = Quantity Nothing Nothing

mkQuantity :: Double -> Quantity
mkQuantity q = case splitQuantity q of
  Nothing -> Quantity Nothing Nothing
  Just (w, d) ->
    case (w == 0, find (\((lo, hi), _) -> lo <= d && d <= hi) knownQuantities) of
      (False, Just (_, (numerator, denominator))) -> Quantity (Just w) (Just (Fraction numerator denominator))
      (True, Just (_, (numerator, denominator))) -> Quantity Nothing (Just (Fraction numerator denominator))
      (False, Nothing) -> Quantity (Just w) Nothing
      (True, Nothing) -> Quantity Nothing Nothing

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

    splitQuantity :: Double -> Maybe (Int, Double)
    splitQuantity q2 =
      case abs (fromIntegral (round q2 :: Int) - q2) < quantityPrecision of
        True -> Just (round q2, 0.0)
        False -> let w = truncate q2 in Just (w, q2 - fromIntegral w)
