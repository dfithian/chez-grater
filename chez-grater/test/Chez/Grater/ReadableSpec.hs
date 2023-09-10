module Chez.Grater.ReadableSpec where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Types (Quantity(..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- the module being tested
import Chez.Grater.Readable.Types

spec :: Spec
spec = describe "Readable" $ do
  describe "Quantity" $ do
    it "shows 1" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 1)
        `shouldBe` Just "1"
    it "shows 1/2" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 0.5)
        `shouldBe` Just "1/2"
    it "shows 1 1/4" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 1.25)
        `shouldBe` Just "1 1/4"
    it "shows 3/8" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 0.375)
        `shouldBe` Just "3/8"
    it "shows 7/9" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 7/9)
        `shouldBe` Just "7/9"
    it "shows (an approximation of) 1 13/100" $
      (showReadableQuantity . mkReadableQuantity) (Quantity 113/100)
        `shouldBe` Just "1 3/23"

    it "can't show missing" $
      (showReadableQuantity . mkReadableQuantity) QuantityMissing
        `shouldBe` Nothing
