module Chez.Grater.Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.CI.Orphans ()
import Data.Aeson (FromJSON, ToJSON)

newtype IngredientName = IngredientName { unIngredientName :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data Quantity
  = Quantity Double
  | QuantityMissing
  deriving (Eq, Ord, Show)

data Unit
  = Unit (CI Text)
  | UnitMissing
  deriving (Eq, Ord, Show)

newtype Step = Step { unStep :: Text }
  deriving (Eq, Ord, Show)

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show)

quantityToValue :: Quantity -> Double
quantityToValue = \case
  Quantity x -> x
  QuantityMissing -> 1

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

instance Num Quantity where
  QuantityMissing + QuantityMissing = QuantityMissing
  x + y = Quantity $ quantityToValue x + quantityToValue y

  QuantityMissing * QuantityMissing = QuantityMissing
  x * y = Quantity $ quantityToValue x * quantityToValue y

  abs = \case
    Quantity x -> Quantity $ abs x
    QuantityMissing -> QuantityMissing

  signum = \case
    Quantity x -> Quantity $ signum x
    QuantityMissing -> QuantityMissing

  fromInteger = Quantity . fromInteger

  negate = \case
    Quantity x -> Quantity $ negate x
    QuantityMissing -> QuantityMissing

instance Fractional Quantity where
  fromRational = Quantity . fromRational

  QuantityMissing / QuantityMissing = QuantityMissing
  x / y = Quantity $ quantityToValue x / quantityToValue y
