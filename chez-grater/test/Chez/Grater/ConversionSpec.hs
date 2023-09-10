module Chez.Grater.ConversionSpec where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Test.ParsedIngredients
  ( allParsedIngredients, pureIngredient, pureIngredientName, pureIngredientNoQuantity
  , pureIngredientNoUnit
  )
import Chez.Grater.Types (Unit(..), cup, ounce, pinch, tablespoon, teaspoon, whole)
import Data.Monoid (Sum(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)
import Test.QuickCheck (forAll, shuffle)
import qualified Data.Map.Strict as Map

-- the module being tested
import Chez.Grater.Conversion

allUnits :: [Unit]
allUnits = [ounce, cup, tablespoon, teaspoon, pinch, Unit "none", whole]

spec :: Spec
spec = describe "Conversion" $ do
  describe "Units" $ do
    it "gets all conversions for ounce" $
      getAllConversions ounce `shouldBe` Map.fromList [(ounce, 1), (cup, 8), (tablespoon, 128), (teaspoon, 384), (pinch, 1536)]
    it "orders correctly" $ forAll (shuffle allUnits) $ \xs ->
      sortBy unitOrdering xs `shouldBe` allUnits
    it "combines quantities - cup equivalent to ounce" $
      combineQuantities (Map.fromList [(cup, Sum 8), (ounce, Sum 1)]) `shouldBe` Map.fromList [(ounce, Sum 2)]
    it "combines quantities - cup less than ounce" $
      combineQuantities (Map.fromList [(cup, Sum 4), (ounce, Sum 1)]) `shouldBe` Map.fromList [(ounce, Sum 1.5)]
    it "combines quantities - teaspoons to tablespoons to cups" $
      combineQuantities (Map.fromList [(teaspoon, Sum 2), (tablespoon, Sum 2), (cup, Sum 4)]) `shouldBe` Map.fromList [(cup, Sum 4.25)]
    it "retains quantities not in known conversions" $
      combineQuantities (Map.fromList [(ounce, Sum 1), (whole, Sum 1)]) `shouldBe` Map.fromList [(ounce, Sum 1), (whole, Sum 1)]

  describe "Ingredients" $ do
    it "combines ingredients" $
      let expected =
            [
            -- combined
              pureIngredient 2.75 "cup" "sliced carrots"
            , pureIngredient (4 / 3) "cup" "chopped onion"
            , pureIngredient ((1 / 3) + (1 / 3) + 1) "cup" "all-purpose flour"
            , pureIngredient 2.75 "tsp" "salt"
            , pureIngredient ((2 / 3) + (1 / 2)) "cup" "milk"
            , pureIngredient 1 "tsp" "pepper"
            , pureIngredient 4.75 "cup" "chicken broth"

            -- allrecipes
            , pureIngredient 1 "pound" "skinless, boneless chicken breast halves - cubed"
            -- , pureIngredient 1 "cup" "sliced carrots"
            , pureIngredient 1 "cup" "frozen green peas"
            , pureIngredient 0.5 "cup" "sliced celery"
            , pureIngredient (1 / 3) "cup" "butter"
            -- , pureIngredient (1 / 3) "cup" "chopped onion"
            -- , pureIngredient (1 / 3) "cup" "all-purpose flour"
            -- , pureIngredient 0.5 "tsp" "salt"
            , pureIngredient 0.25 "tsp" "black pepper"
            , pureIngredient 0.25 "tsp" "celery seed"
            -- , pureIngredient 1.75 "cup" "chicken broth"
            -- , pureIngredient (2 / 3) "cup" "milk"
            , pureIngredientNoUnit 2 "(9 inch) unbaked pie crusts"

            -- pillsbury
            , pureIngredient 1 "box" "(14.1 oz) refrigerated pillsbury\8482 pie crusts (2 count), softened as directed on box"
            , pureIngredient (1 / 3) "cup" "butter or margarine"
            -- , pureIngredient (1 / 3) "cup" "chopped onion"
            -- , pureIngredient (1 / 3) "cup" "all-purpose flour"
            -- , pureIngredient 0.5 "tsp" "salt"
            -- , pureIngredient 0.25 "tsp" "pepper"
            , pureIngredient 1.75 "cup" "progresso\8482 chicken broth (from 32-oz carton)"
            -- , pureIngredient (1 / 2) "cup" "milk"
            , pureIngredient 2.5 "cup" "shredded cooked chicken or turkey"
            , pureIngredient 2 "cup" "frozen mixed vegetables, thawed"

            -- taste of home
            , pureIngredient 2 "cup" "diced peeled potatoes"
            -- , pureIngredient 1.75 "cup" "sliced carrots"
            , pureIngredient 1 "cup" "butter, cubed"
            -- , pureIngredient (2 / 3) "cup" "chopped onion"
            -- , pureIngredient 1 "cup" "all-purpose flour"
            -- , pureIngredient 1.75 "tsp" "salt"
            , pureIngredient 1 "tsp" "dried thyme"
            -- , pureIngredient 0.75 "tsp" "pepper"
            -- , pureIngredient 3 "cup" "chicken broth"
            , pureIngredient 1.5 "cup" "whole milk"
            , pureIngredient 4 "cup" "cubed cooked chicken"
            , pureIngredient 1 "cup" "frozen peas"
            , pureIngredient 1 "cup" "frozen corn"
            , pureIngredientNoUnit 4 "sheets refrigerated pie crust"

            -- rachel mansfield
            , pureIngredient (1 / 3) "cup" "+ 2 tablespoons coconut flour"
            , pureIngredientNoUnit 3 "eggs at room temperature"
            , pureIngredient 1 "tbsp" "maple syrup"
            , pureIngredientNoUnit 3 "medium/large ripe bananas mashed"
            , pureIngredient 1 "tbsp" "melted & cooled coconut oil"
            , pureIngredient 0.5 "tsp" "of baking powder"
            , pureIngredient 0.5 "cup" "of dark chocolate chips"
            , pureIngredient 0.5 "cup" "creamy nut butter"
            , pureIngredientNoQuantity "sprinkle" "of cinnamon"
            , pureIngredientNoQuantity "splash" "of vanilla extract"

            -- food network
            , pureIngredientNoUnit 1 "(5 to 6 pound) roasting chicken"
            , pureIngredientName "kosher salt"
            , pureIngredientName "freshly ground black pepper"
            , pureIngredientNoUnit 1 "large bunch fresh thyme, plus 20 sprigs"
            , pureIngredientNoUnit 1 "lemon, halved"
            , pureIngredientNoUnit 1 "head garlic, cut in half crosswise"
            , pureIngredient 2 "tbsp" "(1/4 stick) butter, melted"
            , pureIngredientNoUnit 1 "large yellow onion, thickly sliced"
            , pureIngredientNoUnit 4 "carrots cut into 2-inch chunks"
            , pureIngredientNoUnit 1 "bulb of fennel, tops removed, and cut into wedges"
            , pureIngredientName "olive oil"
            ]
      in combineItems (mconcat allParsedIngredients) `shouldMatchList` expected
