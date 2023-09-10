module Chez.Grater.Test.Gen where

import Chez.Grater.Internal.Prelude

import Test.QuickCheck (Gen, arbitrary, elements, oneof)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text

import Chez.Grater.Readable.Types
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

arbitraryReadableFraction :: Gen ReadableFraction
arbitraryReadableFraction = ReadableFraction
  <$> arbitraryInt
  <*> ((+1) <$> arbitraryInt)

arbitraryReadableQuantity :: Gen ReadableQuantity
arbitraryReadableQuantity = ReadableQuantity
  <$> maybeGen arbitraryInt
  <*> maybeGen arbitraryReadableFraction

arbitraryReadableUnit :: Gen ReadableUnit
arbitraryReadableUnit = ReadableUnit <$> arbitraryCi
