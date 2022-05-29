module Cheez.ParserSpec where

import Cheez.Prelude

import Cheez.ParsedIngredients
  ( allRecipesIngredients, foodNetworkIngredients, pillsburyIngredients, rachelMansfieldIngredients
  , tasteOfHomeIngredients
  )
import Cheez.Types (IngredientName(..), RawIngredient(..), RawQuantity(..), RawUnit(..))
import Data.FileEmbed (embedFile)
import System.FilePath.TH (fileRelativeToAbsoluteStr)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldMatchList)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- the module being tested
import Cheez.Parser

parseStrict :: (Eq a, Show a) => a -> Atto.Parser a -> Text -> Expectation
parseStrict expected parser input = Atto.parseOnly parser input `shouldBe` Right expected

spec :: Spec
spec = describe "Parser" $ do
  describe "Examples" $ do
    it "can parse a unit" $ parseStrict (RawUnit "ounces") unitP " ounces "
    it "can parse a fraction" $ parseStrict (RawQuantity $ 1 / 3) quantityP "1/3"
    it "can parse an improper fraction" $ parseStrict (RawQuantity 1.5) quantityP "1-1/2"
    it "can parse an improper fraction with spaces" $ parseStrict (RawQuantity 1.5) quantityP "1 1/2"
    it "can parse a range" $ parseStrict (RawQuantity 2.5) quantityP "2-3"
    it "can parse a decimal" $ parseStrict (RawQuantity 0.25) quantityP "0.25"
    it "can parse a word" $ parseStrict (RawQuantityWord "half") quantityP "\nhalf\n"
    it "can parse an ingredient name" $ parseStrict (IngredientName "chicken") nameP " chicken"
    it "can parse \"chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing RawUnitMissing) ingredientP "chicken"
    it "can parse \"one chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") RawUnitMissing) ingredientP "one chicken"
    it "can parse \"whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") RawQuantityMissing (RawUnit "whole")) ingredientP "whole chicken"
    it "can parse \"one whole chicken\"" $ parseStrict (RawIngredient (IngredientName "chicken") (RawQuantityWord "one") (RawUnit "whole")) ingredientP "one whole chicken"
    it "can parse \"1/4 cup broth\"" $ parseStrict (RawIngredient (IngredientName "broth") (RawQuantity 0.25) (RawUnit "cup")) ingredientP "1/4\ncup\nbroth"

  describe "Paste" $ do
    it "can parse allrecipes" $ do
      actual <- either (fail . Text.unpack) pure $ parseRawIngredients $ Text.decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../fixtures/chicken-pot-pie-allrecipes.txt")
      actual `shouldMatchList` allRecipesIngredients

    it "can parse pillsbury" $ do
      actual <- either (fail . Text.unpack) pure $ parseRawIngredients $ Text.decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../fixtures/chicken-pot-pie-pillsbury.txt")
      actual `shouldMatchList` pillsburyIngredients

    it "can parse taste of home" $ do
      actual <- either (fail . Text.unpack) pure $ parseRawIngredients $ Text.decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../fixtures/chicken-pot-pie-tasteofhome.txt")
      actual  `shouldMatchList` tasteOfHomeIngredients

    it "can parse rachel mansfield" $ do
      actual <- either (fail . Text.unpack) pure $ parseRawIngredients $ Text.decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../fixtures/banana-bread-rachelmansfield.txt")
      actual  `shouldMatchList` rachelMansfieldIngredients

    it "can parse food network" $ do
      actual <- either (fail . Text.unpack) pure $ parseRawIngredients $ Text.decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../fixtures/roast-chicken-food-network.txt")
      actual `shouldMatchList` foodNetworkIngredients
