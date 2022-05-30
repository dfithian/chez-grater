module Chez.Grater.Parser.Types where

import Chez.Grater.Internal.Prelude

import GHC.Generics (Generic)

newtype ParsedIngredientName = ParsedIngredientName { unParsedIngredientName :: CI Text }
  deriving (Eq, Ord, Show, Generic)

data ParsedQuantity
  = ParsedQuantity Double
  -- ^The quantity is a number.
  | ParsedQuantityWord (CI Text)
  -- ^The quantity is a word.
  | ParsedQuantityMissing
  -- ^There was no detected quantity.
  deriving (Eq, Ord, Show)

data ParsedUnit
  = ParsedUnit (CI Text)
  -- ^Detected a unit.
  | ParsedUnitMissing
  -- ^There was no detected unit.
  deriving (Eq, Ord, Show)

data ParsedIngredient = ParsedIngredient
  { parsedIngredientName     :: ParsedIngredientName
  , parsedIngredientQuantity :: ParsedQuantity
  , parsedIngredientUnit     :: ParsedUnit
  }
  deriving (Eq, Ord, Show)
