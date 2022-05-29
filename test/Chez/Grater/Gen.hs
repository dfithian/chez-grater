module Chez.Grater.Gen where

import Chez.Grater.Prelude

import Test.QuickCheck (Gen, arbitrary, elements, oneof)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text

import Chez.Grater.Types

arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

arbitraryAlphaNumStr :: Gen Text
arbitraryAlphaNumStr = do
  n <- arbitrary
  Text.pack <$> replicateM (abs n + 10) arbitraryAlphaNum

arbitraryCi :: Gen (CI Text)
arbitraryCi = CI.mk <$> arbitraryAlphaNumStr

arbitraryIngredientName :: Gen IngredientName
arbitraryIngredientName = IngredientName <$> arbitraryCi

arbitraryRecipeName :: Gen RecipeName
arbitraryRecipeName = RecipeName <$> arbitraryAlphaNumStr

trunc :: Int -> Double -> Double
trunc n x = (fromIntegral (floor (x * t) :: Int)) / t
  where t = 10^n

arbitraryDouble :: Gen Double
arbitraryDouble = trunc 5 . abs <$> arbitrary

arbitraryQuantity :: Gen Quantity
arbitraryQuantity = oneof
  [ pure QuantityMissing
  , Quantity <$> arbitraryDouble
  ]

arbitraryUnit :: Gen Unit
arbitraryUnit = oneof
  [ pure UnitMissing
  , Unit <$> arbitraryCi
  ]

arbitraryIngredient :: Gen Ingredient
arbitraryIngredient = Ingredient
  <$> arbitraryIngredientName
  <*> arbitraryQuantity
  <*> arbitraryUnit

arbitraryStep :: Gen Step
arbitraryStep = Step
  <$> arbitraryAlphaNumStr
