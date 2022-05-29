module Cheez.Combinable where

import Cheez.Prelude

import Cheez.Types (Ingredient(..), IngredientName, Quantity, Unit)

newtype Constant a = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Semigroup (Constant a) where
  x <> _ = x

class Combinable a where
  type Sidecar a :: *
  mk :: a -> (IngredientName, Quantity, Unit, Sidecar a)
  orig :: (IngredientName, Quantity, Unit, Sidecar a) -> a

instance Combinable Ingredient where
  type Sidecar Ingredient = ()
  mk Ingredient {..} = (ingredientName, ingredientQuantity, ingredientUnit, ())
  orig (ingredientName, ingredientQuantity, ingredientUnit, ()) = Ingredient {..}
