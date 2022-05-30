module Chez.GraterSpec where

import Chez.Grater.Internal.Prelude

import Chez.Grater.ParsedIngredients
  ( allRecipesIngredients, allRecipesSteps, bettyCrockerIngredients, bettyCrockerSteps
  , cafeDelitesIngredients, cafeDelitesSteps, eatingWellIngredients, eatingWellSteps
  , foodIngredients, foodNetworkIngredients, foodNetworkSteps, foodSteps, pillsburyIngredients
  , pillsburySteps, rachelMansfieldIngredients, rachelMansfieldSteps, sallysBakingIngredients
  , sallysBakingSteps, tasteOfHomeIngredients, tasteOfHomeSteps
  )
import Chez.Grater.Scraper.Types ()
import Chez.Grater.TestEnv (Env(..))
import Chez.Grater.Types
  ( Ingredient(..), IngredientName(..), RecipeName(..), Step(..), emptyQuantity
  )
import Control.Monad (when)
import Data.List (intercalate)
import Network.URI (parseURI)
import Test.Hspec
  ( Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldMatchList, shouldSatisfy
  , xit
  )
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

-- the module being tested
import Chez.Grater

data TestCfg = TestCfg
  { requireOneQuantityUnit :: Bool
  , allowedDuplicates :: Int
  , requiredIngredients :: Int
  , requiredSteps :: Int
  , env :: Env
  }

defaultTestCfg :: Env -> TestCfg
defaultTestCfg env = TestCfg
  { requireOneQuantityUnit = True
  , allowedDuplicates = 3
  , requiredIngredients = 5
  , requiredSteps = 1
  , env = env
  }

scrapeAndParse :: Env -> String -> String -> ([Ingredient], [Step]) -> Expectation
scrapeAndParse Env {..} url expectedName (expectedIngredients, expectedSteps) = do
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  (name, ingredients, steps, _) <- scrapeAndParseUrl envManager uri
  name `shouldBe` RecipeName (Text.pack expectedName)
  ingredients `shouldMatchList` expectedIngredients
  steps `shouldMatchList` expectedSteps

scrapeAndParseConfig :: TestCfg -> String -> Expectation
scrapeAndParseConfig TestCfg {..} url = do
  let Env {..} = env
  uri <- maybe (fail "Invalid URL") pure $ parseURI url
  (name, ingredients, steps, _) <- scrapeAndParseUrl envManager uri
  unRecipeName name `shouldSatisfy` not . Text.null
  ingredients `shouldSatisfy` (\xs -> length xs >= requiredIngredients)
  ingredients `shouldSatisfy` any hasQuantityAndUnit
  ingredients `shouldSatisfy` duplicates
  lessThanThreePrefixes ingredients
  steps `shouldSatisfy` (\xs -> length xs >= requiredSteps)
  where
    hasQuantityAndUnit Ingredient {..} = if requireOneQuantityUnit then ingredientQuantity /= emptyQuantity && ingredientUnit /= Nothing else True
    duplicates = (< allowedDuplicates) . length . filter ((> 1) . length . snd) . Map.toList . foldr (\x@Ingredient {..} -> Map.insertWith (<>) ingredientName [x]) mempty
    lessThanThreePrefixes xs = do
      let names = ingredientName <$> xs
          toStr = Text.toLower . CI.original . unIngredientName
          go x y = case name `Text.isPrefixOf` otherName && name /= otherName of
            True -> [Text.unpack name <> " is a prefix of " <> Text.unpack otherName]
            False -> []
            where
              name = toStr x
              otherName = toStr y
          prefixes = [ z | x <- names, y <- names, z <- go x y ]
      when (length prefixes >= 3) $
        expectationFailure $ intercalate ", " prefixes

spec :: Env -> Spec
spec env = describe "Scrape" $ do
  let defCfg = defaultTestCfg env
  describe "Examples" $ do
    it "can parse allrecipes" $
      scrapeAndParse
        env
        "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
        "Chicken Pot Pie IX Recipe | Allrecipes"
        (allRecipesIngredients, allRecipesSteps)

    it "can parse food" $
      scrapeAndParse
        env
        "https://www.food.com/recipe/hearty-tuscan-white-bean-soup-192495"
        "Hearty Tuscan White Bean Soup Recipe - Food.com"
        (foodIngredients, foodSteps)

    it "can parse pillsbury" $
      scrapeAndParse
        env
        "https://www.pillsbury.com/recipes/classic-chicken-pot-pie/1401d418-ac0b-4b50-ad09-c6f1243fb992"
        "Classic Chicken Pot Pie Recipe - Pillsbury.com"
        (pillsburyIngredients, pillsburySteps)

    it "can parse betty crocker" $
      scrapeAndParse
        env
        "https://www.bettycrocker.com/recipes/mississippi-mud-brownies/dff02c0e-695b-4b01-90fd-7071ddb84457"
        "Mississippi Mud Brownies Recipe - BettyCrocker.com"
        (bettyCrockerIngredients, bettyCrockerSteps)

    it "can parse taste of home" $
      scrapeAndParse
        env
        "https://www.tasteofhome.com/recipes/favorite-chicken-potpie/"
        "Favorite Chicken Potpie Recipe: How to Make It"
        (tasteOfHomeIngredients, tasteOfHomeSteps)

    it "can parse rachel mansfield" $
      scrapeAndParse
        env
        "https://rachlmansfield.com/paleo-chocolate-chip-banana-bread/"
        "Paleo Chocolate Chip Banana Bread (Nut Free) - rachLmansfield"
        (rachelMansfieldIngredients, rachelMansfieldSteps)

    it "can parse food network" $
      scrapeAndParse
        env
        "https://www.foodnetwork.com/recipes/ina-garten/perfect-roast-chicken-recipe-1940592"
        "Perfect Roast Chicken Recipe | Ina Garten | Food Network"
        (foodNetworkIngredients, foodNetworkSteps)

    it "can parse sallys baking" $
      scrapeAndParse
        env
        "https://sallysbakingaddiction.com/chocolate-lava-cakes/"
        "How to Make Chocolate Lava Cakes - Sally's Baking Addiction"
        (sallysBakingIngredients, sallysBakingSteps)

    it "can parse cafe delites" $
      scrapeAndParse
        env
        "https://cafedelites.com/chicken-tikka-masala/"
        "Chicken Tikka Masala - Cafe Delites"
        (cafeDelitesIngredients, cafeDelitesSteps)

    it "can parse eatingwell" $
      scrapeAndParse
        env
        "https://www.eatingwell.com/recipe/7898240/baked-spinach-feta-pasta/"
        "Baked Spinach & Feta Pasta Recipe | EatingWell"
        (eatingWellIngredients, eatingWellSteps)

  describe "Smoke Test" $ do
    it "handles nytimes" $ scrapeAndParseConfig defCfg "https://cooking.nytimes.com/recipes/1017256-french-onion-soup"
    it "handles yummly" $ scrapeAndParseConfig defCfg "https://www.yummly.com/recipe/Barbecue-Baked-Chicken-Legs-9073054"
    -- this one can't parse anymore
    xit "handles epicurious" $ scrapeAndParseConfig defCfg "https://www.epicurious.com/recipes/food/views/cashew-chicken"
    it "handles tasty" $ scrapeAndParseConfig defCfg "https://tasty.co/recipe/cilantro-lime-chicken-veggie-rice-meal-prep"
    it "handles delish" $ scrapeAndParseConfig defCfg "https://www.delish.com/cooking/recipe-ideas/a27469808/acai-bowl-recipe/"
    it "handles delish" $ scrapeAndParseConfig defCfg "https://www.delish.com/cooking/a36146989/vegan-tofu-grain-bowl/"
    it "handles spoonacular" $ scrapeAndParseConfig (defCfg { requiredSteps = 0 }) "https://spoonacular.com/recipes/chocolate-chip-cookie-bars-1518975"
    it "handles cookieandkate" $ scrapeAndParseConfig defCfg "https://cookieandkate.com/cream-of-broccoli-soup-recipe/"
    it "handles budgetbytes" $ scrapeAndParseConfig defCfg "https://www.budgetbytes.com/spaghetti-with-vegetable-meat-sauce/"
    it "handles daringgourmet" $ scrapeAndParseConfig defCfg "https://www.daringgourmet.com/hamburger-gravy/"
    it "handles damndelicious" $ scrapeAndParseConfig defCfg "https://damndelicious.net/2020/12/04/creamy-chicken-and-gnocchi/"
    it "handles gimmesomeoven" $ scrapeAndParseConfig defCfg "https://www.gimmesomeoven.com/best-caesar-salad/"
    it "handles recipetineats" $ scrapeAndParseConfig defCfg "https://www.recipetineats.com/orecchiette-sausage-pasta-in-creamy-tomato-sauce/"
    it "handles cookingclassy" $ scrapeAndParseConfig defCfg "https://www.cookingclassy.com/sheet-pan-shrimp-asparagus/"
    it "handles natashaskitchen" $ scrapeAndParseConfig defCfg "https://natashaskitchen.com/oven-roasted-baby-red-potatoes/"
    it "handles pinchofyum" $ scrapeAndParseConfig defCfg "https://pinchofyum.com/spicy-chicken-sweet-potato-meal-prep-magic"
    it "handles justonecookbook" $ scrapeAndParseConfig defCfg "https://www.justonecookbook.com/teriyaki-pork-donburi/"
    it "handles loveandlemons" $ scrapeAndParseConfig defCfg "https://www.loveandlemons.com/artichoke-dipping-sauce/"
    it "handles foodiecrush" $ scrapeAndParseConfig defCfg "https://www.foodiecrush.com/strawberry-and-avocado-spinach-salad-with-chicken/"
    it "handles therecipecritic" $ scrapeAndParseConfig defCfg "https://therecipecritic.com/hot-spinach-artichoke-dip/"
    it "handles ambitiouskitchen" $ scrapeAndParseConfig defCfg "https://www.ambitiouskitchen.com/coconut-chocolate-peanut-butter-protein-bars/"
    it "handles melskitchencafe" $ scrapeAndParseConfig defCfg "https://www.melskitchencafe.com/easy-chicken-enchilada-casserole/"
    it "handles halfbakedharvest" $ scrapeAndParseConfig defCfg "https://www.halfbakedharvest.com/southern-butter-biscuits/"
    it "handles simpleveganblog" $ scrapeAndParseConfig defCfg "https://simpleveganblog.com/vegan-stuffed-peppers/"
    it "handles smittenkitchen" $ scrapeAndParseConfig defCfg "https://smittenkitchen.com/2021/04/spring-asparagus-galette/"
    it "handles 101cookbooks" $ scrapeAndParseConfig defCfg "https://www.101cookbooks.com/mushroom-scallion-tartine/"
    it "handles ohsweetbasil" $ scrapeAndParseConfig defCfg "https://ohsweetbasil.com/quick-grilled-chicken-with-oregaon-recipe/"
    it "handles myfoodstory" $ scrapeAndParseConfig defCfg "https://myfoodstory.com/prawn-rava-fry/"
    it "handles easypeasyfoodie" $ scrapeAndParseConfig defCfg "https://www.easypeasyfoodie.com/coffee-and-walnut-traybake/"
    it "handles veganricha" $ scrapeAndParseConfig defCfg "https://www.veganricha.com/potato-quinoa-waffles-aloo-tikki-waffles/"
    it "handles simplydeliciousfood" $ scrapeAndParseConfig defCfg "https://simply-delicious-food.com/general-tsos-chicken/"
    it "handles lexiscleankitchen" $ scrapeAndParseConfig defCfg "https://lexiscleankitchen.com/buffalo-chicken-dip/"
    it "handles lazycatkitchen" $ scrapeAndParseConfig defCfg "https://www.lazycatkitchen.com/vegan-blondies/"
    it "handles alexandracooks" $ scrapeAndParseConfig defCfg "https://alexandracooks.com/2020/06/25/easy-homemade-pita-bread-recipe/"
    it "handles deliciouslyella" $ scrapeAndParseConfig defCfg "https://deliciouslyella.com/recipes/sweet-potato-black-bean-shepherds-pie/"
    it "handles deliciouseveryday" $ scrapeAndParseConfig defCfg "https://www.deliciouseveryday.com/thai-pumpkin-soup-recipe/"
    it "handles eatyourselfskinny" $ scrapeAndParseConfig defCfg "https://www.eatyourselfskinny.com/easy-beef-stroganoff-casserole/"
    -- times out in gh actions
    xit "handles iamafoodblog" $ scrapeAndParseConfig defCfg "https://iamafoodblog.com/smashed-brussel-sprouts/"
    it "handles naturallyella" $ scrapeAndParseConfig defCfg "https://naturallyella.com/roasted-sweet-potato-sorghum-salad/"
    it "handles glutenfreecuppatea" $ scrapeAndParseConfig defCfg "https://glutenfreecuppatea.co.uk/2021/04/13/toblerone-millionaires-shortbread-recipe/"
    it "handles thelastfoodblog" $ scrapeAndParseConfig defCfg "https://www.thelastfoodblog.com/spinach-and-ricotta-cannelloni/"
    it "handles hemsleyandhemsley" $ scrapeAndParseConfig defCfg "https://hemsleyandhemsley.com/recipe/apple-rocket-and-feta-buckwheat-galettes/"
    -- certificate is invalid
    xit "handles localmilkblog" $ scrapeAndParseConfig defCfg "https://localmilkblog.com/2019/11/turkey-buttermilk-sage-dumpling-soup.html"
    it "handles thefoodblog" $ scrapeAndParseConfig defCfg "https://www.thefoodblog.net/air-fryer-salmon-recipe/"
    it "handles onceuponafoodblog" $ scrapeAndParseConfig defCfg "https://onceuponafoodblog.com/cheesy-bacon-spring-greens/"
    it "handles anotherfoodblogger" $ scrapeAndParseConfig defCfg "https://www.anotherfoodblogger.com/recipes/meatball-marinara/#wprm-recipe-container-6560"
    it "handles brownedbutterblondie" $ scrapeAndParseConfig defCfg "https://brownedbutterblondie.com/the-best-blueberry-streusel-muffins/#tasty-recipes-7789"
    it "handles seriouseats" $ scrapeAndParseConfig defCfg "https://www.seriouseats.com/recipes/2014/03/the-best-black-bean-burger-recipe.html"
    it "handles food52" $ scrapeAndParseConfig defCfg "https://food52.com/recipes/21960-grilled-corn-with-basil-butter"
    it "handles thekitchn" $ scrapeAndParseConfig (defCfg { requiredIngredients = 3 }) "https://www.thekitchn.com/mint-julep-mocktail-recipe-23015618"
    it "handles simplyrecipes" $ scrapeAndParseConfig defCfg "https://www.simplyrecipes.com/recipes/cauliflower_steak_sandwiches_with_red_pepper_aioli/"
    it "handles minimalistbaker" $ scrapeAndParseConfig defCfg "https://minimalistbaker.com/creamy-roasted-cauliflower-soup/"
    it "handles davidlebovitz" $ scrapeAndParseConfig defCfg "https://www.davidlebovitz.com/kofta-kefta-meat-beef-lamb-plant-based-sausage-persian-recipe-spiced/"
    it "handles thepioneerwoman" $ scrapeAndParseConfig defCfg "https://www.thepioneerwoman.com/food-cooking/recipes/a35880840/fried-pickles-recipe/"
    it "handles skinnytaste" $ scrapeAndParseConfig defCfg "https://www.skinnytaste.com/strawberries-and-cream/"
    it "handles twopeasandtheirpod" $ scrapeAndParseConfig defCfg "https://www.twopeasandtheirpod.com/shaved-brussels-sprouts-salad/"
    it "handles slenderkitchen" $ scrapeAndParseConfig defCfg "https://www.slenderkitchen.com/recipe/sunday-slow-cooker-saag-paneer"
    it "handles everydayannie" $ scrapeAndParseConfig defCfg "https://everydayannie.com/2020/12/28/raspberry-cheesecake-streusel-bars/"
    -- 500 internal server error
    xit "handles notwithoutsalt" $ scrapeAndParseConfig defCfg "http://notwithoutsalt.com/brussels-sprout-green-apple-slaw-pickled-cranberries/"
    it "handles chefspencil" $ scrapeAndParseConfig (defCfg { requiredSteps = 0 }) "https://www.chefspencil.com/recipe/carrot-tarte-tatin/"
    it "handles sweetandsavorymeals" $ scrapeAndParseConfig defCfg "https://sweetandsavorymeals.com/air-fryer-eggplant/"
    it "handles eatwell101" $ scrapeAndParseConfig (defCfg { requiredSteps = 0 }) "https://www.eatwell101.com/garlic-butter-chicken-bites-asparagus-recipe"

  describe "Implicit" $ do
    describe "WPRM" $ do
      it "handles downshiftology" $ scrapeAndParseConfig defCfg "https://downshiftology.com/recipes/chicken-salad/"
      it "handles altonbrown" $ scrapeAndParseConfig (defCfg { requiredIngredients = 4 }) "https://altonbrown.com/recipes/2-note-mousse/"
      it "handles spoonforkbacon" $ scrapeAndParseConfig (defCfg { allowedDuplicates = 4 }) "https://www.spoonforkbacon.com/chicken-and-dumplings/#wprm-recipe-container-40527"
      -- uses a captcha
      xit "handles dinnerthendessert" $ scrapeAndParseConfig defCfg "https://dinnerthendessert.com/ultimate-slow-cooker-pot-roast/"
      it "handles howsweeteats" $ scrapeAndParseConfig defCfg "https://www.howsweeteats.com/2021/04/pimento-cheese-poppers/"
      it "handles browneyedbaker" $ scrapeAndParseConfig defCfg "https://www.browneyedbaker.com/cannoli/"
      it "handles steamykitchen" $ scrapeAndParseConfig defCfg "https://steamykitchen.com/59396-shrimp-chow-mein.html"
      it "handles sweetashoney" $ scrapeAndParseConfig defCfg "https://www.sweetashoney.co/keto-french-baguette-recipe/"
      it "handles thestayathomechef" $ scrapeAndParseConfig defCfg "https://thestayathomechef.com/taco-casserole/"
      it "handles cookilicious" $ scrapeAndParseConfig defCfg "https://cookilicious.com/condiments/andhra-style-peanut-tomato-chutney-recipe-for-idli-dosa/"
      it "handles ohmyveggies" $ scrapeAndParseConfig defCfg "https://ohmyveggies.com/vegan-snickerdoodles/"
      it "handles thevanillabeanblog" $ scrapeAndParseConfig defCfg "https://www.thevanillabeanblog.com/2021/03/devils-food-cake-buttercream-chocolate-ganache.html"
      it "handles sugarfreelondoner" $ scrapeAndParseConfig defCfg "https://sugarfreelondoner.com/keto-breakfast-cookies/#wprm-recipe-container-20704"
    describe "Ingredient List" $ do
      it "handles loveandoliveoil" $ scrapeAndParseConfig defCfg "https://www.loveandoliveoil.com/2021/04/stuffed-cherry-amaretti-cookies.html"
      it "handles ohsheglows" $ scrapeAndParseConfig defCfg "https://ohsheglows.com/2020/09/20/perfect-little-pumpkin-cookies-with-spiced-buttercream/"
      it "handles shutterbean" $ scrapeAndParseConfig (defCfg { requiredSteps = 0 }) "https://www.shutterbean.com/2019/polenta-cornbread/"
    describe "MV" $ do
      it "handles bakerella" $ scrapeAndParseConfig defCfg "https://www.bakerella.com/secret-ingredient-chocolate-chip-cookies/"
      it "handles mybakingaddiction" $ scrapeAndParseConfig defCfg "https://www.mybakingaddiction.com/flank-steak-tacos/"
    describe "ZL" $ do
      it "handles cnz" $ scrapeAndParseConfig defCfg "https://cnz.to/recipes/meat-charcuterie/instant-pot-ramen-style-pork-belly-recipe/"
    describe "Tasty 2" $ do
      it "handles joythebaker" $ scrapeAndParseConfig defCfg "https://joythebaker.com/2019/03/chili-and-cheese-buttery-biscuits/"
    describe "Tasty 3" $ do
      it "handles ourbestbites" $ scrapeAndParseConfig (defCfg { requiredIngredients = 1 }) "https://ourbestbites.com/greek-pasta-salad/#tasty-recipes-45657"
    describe "Tasty 4" $ do
      it "handles simple-veganista" $ scrapeAndParseConfig (defCfg { requiredIngredients = 1 }) "https://simple-veganista.com/vegan-stuffed-peppers/#tasty-recipes-28732-jump-target"
    describe "Eating Well" $ do
      it "handles bhg" $ scrapeAndParseConfig (defCfg { requireOneQuantityUnit = False }) "https://www.bhg.com/recipe/air-fried-ginger-glazed-pork-ribs/"
    describe "Simply Recipes" $ do
      it "handles simplyrecipes" $ scrapeAndParseConfig defCfg "https://www.simplyrecipes.com/recipes/easy_green_chicken_chili/"
