module Chez.Grater.Gen where

import Chez.Grater.Internal.Prelude

import Test.QuickCheck (Gen, arbitrary, elements, oneof)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text

import Chez.Grater.Types

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen gen = oneof
  [ Just <$> gen
  , pure Nothing
  ]

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

arbitraryInt :: Gen Int
arbitraryInt = abs <$> arbitrary

arbitraryFraction :: Gen Fraction
arbitraryFraction = Fraction
  <$> arbitraryInt
  <*> ((+1) <$> arbitraryInt)

arbitraryQuantity :: Gen Quantity
arbitraryQuantity = Quantity
  <$> maybeGen arbitraryInt
  <*> maybeGen arbitraryFraction

arbitraryUnit :: Gen (Maybe Unit)
arbitraryUnit = maybeGen (Unit <$> arbitraryCi)

arbitraryIngredient :: Gen Ingredient
arbitraryIngredient = Ingredient
  <$> arbitraryIngredientName
  <*> arbitraryQuantity
  <*> arbitraryUnit

arbitraryStep :: Gen Step
arbitraryStep = Step
  <$> arbitraryAlphaNumStr
