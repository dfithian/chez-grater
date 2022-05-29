module Cheez.Types where

import Cheez.Prelude

import Cheez.CI.Orphans ()
import Cheez.Json (jsonOptions)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Ord, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey, FromHttpApiData, ToHttpApiData)

newtype IngredientName = IngredientName { unIngredientName :: CI Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype RecipeName = RecipeName { unRecipeName :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data RawQuantity
  = RawQuantity Double
  | RawQuantityWord (CI Text)
  | RawQuantityMissing
  deriving (Eq, Ord, Show)

data Quantity
  = Quantity Double
  | QuantityMissing
  deriving (Eq, Ord, Show, Generic)

quantityToValue :: Quantity -> Double
quantityToValue = \case
  Quantity x -> x
  QuantityMissing -> 1

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data RawUnit
  = RawUnit (CI Text)
  | RawUnitMissing
  deriving (Eq, Ord, Show)

data Unit
  = Unit (CI Text)
  | UnitMissing
  deriving (Eq, Ord, Show, Generic)

data ReadableFraction = ReadableFraction
  { readableFractionNumerator   :: Int
  , readableFractionDenominator :: Int
  }
  deriving (Eq, Show, Ord)

data ReadableQuantity = ReadableQuantity
  { readableQuantityWhole    :: Maybe Int
  , readableQuantityFraction :: Maybe ReadableFraction
  }
  deriving (Eq, Show, Ord)

newtype ReadableUnit = ReadableUnit { unReadableUnit :: CI Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data Ingredient = Ingredient
  { ingredientName     :: IngredientName
  , ingredientQuantity :: Quantity
  , ingredientUnit     :: Unit
  }
  deriving (Eq, Ord, Show, Generic)

data OrderedIngredient = OrderedIngredient
  { orderedIngredientIngredient :: Ingredient
  , orderedIngredientOrder      :: Int
  }
  deriving (Eq, Ord, Show)

data RawIngredient = RawIngredient
  { rawIngredientName     :: IngredientName
  , rawIngredientQuantity :: RawQuantity
  , rawIngredientUnit     :: RawUnit
  }
  deriving (Eq, Ord, Show)

newtype Step = Step { unStep :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data ReadableIngredient = ReadableIngredient
  { readableIngredientName     :: IngredientName
  , readableIngredientQuantity :: ReadableQuantity
  , readableIngredientUnit     :: Maybe ReadableUnit
  , readableIngredientOrder    :: Int
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "readableFraction") ''ReadableFraction
deriveJSON (jsonOptions "readableQuantity") ''ReadableQuantity
deriveJSON (jsonOptions "readableIngredient") ''ReadableIngredient

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
