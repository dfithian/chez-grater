module Chez.Grater.Test.ParsedIngredients where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Types (Ingredient(..), IngredientName(..), Quantity(..), Unit(..))
import qualified Data.CaseInsensitive as CI

allParsedIngredients :: [[Ingredient]]
allParsedIngredients =
  [ allRecipesIngredients
  , pillsburyIngredients
  , tasteOfHomeIngredients
  , rachelMansfieldIngredients
  , foodNetworkIngredients
  ]

pureIngredient :: Double -> Text -> Text -> Ingredient
pureIngredient q u i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = Quantity q
  , ingredientUnit = Unit $ CI.mk u
  }

pureIngredientNoQuantity :: Text -> Text -> Ingredient
pureIngredientNoQuantity u i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = QuantityMissing
  , ingredientUnit = Unit $ CI.mk u
  }

pureIngredientNoUnit :: Double -> Text -> Ingredient
pureIngredientNoUnit q i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = Quantity q
  , ingredientUnit = UnitMissing
  }

pureIngredientName :: Text -> Ingredient
pureIngredientName i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = QuantityMissing
  , ingredientUnit = UnitMissing
  }

allRecipesIngredients :: [Ingredient]
allRecipesIngredients =
  [ pureIngredient 1 "pound" "skinless, boneless chicken breast halves - cubed"
  , pureIngredient 1 "cup" "sliced carrots"
  , pureIngredient 1 "cup" "frozen green peas"
  , pureIngredient 0.5 "cup" "sliced celery"
  , pureIngredient (1 / 3) "cup" "butter"
  , pureIngredient (1 / 3) "cup" "chopped onion"
  , pureIngredient (1 / 3) "cup" "all-purpose flour"
  , pureIngredient 0.5 "tsp" "salt"
  , pureIngredient 0.25 "tsp" "black pepper"
  , pureIngredient 0.25 "tsp" "celery seed"
  , pureIngredient 1.75 "cup" "chicken broth"
  , pureIngredient (2 / 3) "cup" "milk"
  , pureIngredientNoUnit 2 "(9 inch) unbaked pie crusts"
  ]

foodIngredients :: [Ingredient]
foodIngredients =
  [ pureIngredientNoUnit 1 "(10 ounce) packaged frozen chopped spinach, thawed"
  , pureIngredientNoUnit 1 "(14 ounce) can diced tomatoes, undrained"
  , pureIngredientNoUnit 3 "(14 ounce) cans chicken broth"
  , pureIngredientNoUnit 2 "(15 ounce) cans white beans, any variety"
  , pureIngredientNoUnit 1 "bay leaf"
  , pureIngredientNoUnit 2 "carrots, chopped"
  , pureIngredientNoUnit 4 "garlic cloves, minced"
  , pureIngredientName "grated parmesan cheese, and or pecorino romano cheese, to serve"
  , pureIngredientNoUnit 1 "medium onion, chopped"
  , pureIngredientName "salt and pepper"
  , pureIngredientNoUnit 2 "stalks celery, chopped"
  , pureIngredient 0.75 "cup" "macaroni, uncooked"
  , pureIngredient 0.25 "cup" "olive oil"
  , pureIngredient 0.25 "tsp" "dried thyme"
  ]

pillsburyIngredients :: [Ingredient]
pillsburyIngredients =
  [ pureIngredient 1 "box" "(14.1 oz) refrigerated pillsbury\8482 pie crusts (2 count), softened as directed on box"
  , pureIngredient (1 / 3) "cup" "butter or margarine"
  , pureIngredient (1 / 3) "cup" "chopped onion"
  , pureIngredient (1 / 3) "cup" "all-purpose flour"
  , pureIngredient 0.5 "tsp" "salt"
  , pureIngredient 0.25 "tsp" "pepper"
  , pureIngredient 1.75 "cup" "progresso\8482 chicken broth (from 32-oz carton)"
  , pureIngredient 0.5 "cup" "milk"
  , pureIngredient 2.5 "cup" "shredded cooked chicken or turkey"
  , pureIngredient 2 "cup" "frozen mixed vegetables, thawed"
  ]

bettyCrockerIngredients :: [Ingredient]
bettyCrockerIngredients =
  [ pureIngredient 1 "box" "(18.3 oz) Betty Crocker\8482 traditional fudge brownie mix"
  , pureIngredient 1 "cup" "Betty Crocker\8482 Rich & Creamy vanilla frosting (from 16-oz container)"
  , pureIngredient 2 "cup" "Cool Whip frozen whipped topping (from 8-oz container), thawed"
  , pureIngredient (1 / 3) "cup" "heavy whipping cream"
  , pureIngredient 1 "cup" "miniature marshmallows"
  , pureIngredientNoUnit 20 "Oreo chocolate sandwich cookies, coarsely chopped (about 2 2/3 cups)"
  , pureIngredient 0.5 "cup" "semisweet chocolate chips"
  , pureIngredientName "Water, vegetable oil and eggs called for on brownie mix box for cakelike brownies"
  ]

tasteOfHomeIngredients :: [Ingredient]
tasteOfHomeIngredients =
  [ pureIngredient 2 "cup" "diced peeled potatoes"
  , pureIngredient 1.75 "cup" "sliced carrots"
  , pureIngredient 1 "cup" "butter, cubed"
  , pureIngredient (2 / 3) "cup" "chopped onion"
  , pureIngredient 1 "cup" "all-purpose flour"
  , pureIngredient 1.75 "tsp" "salt"
  , pureIngredient 1 "tsp" "dried thyme"
  , pureIngredient 0.75 "tsp" "pepper"
  , pureIngredient 3 "cup" "chicken broth"
  , pureIngredient 1.5 "cup" "whole milk"
  , pureIngredient 4 "cup" "cubed cooked chicken"
  , pureIngredient 1 "cup" "frozen peas"
  , pureIngredient 1 "cup" "frozen corn"
  , pureIngredientNoUnit 4 "sheets refrigerated pie crust"
  ]

rachelMansfieldIngredients :: [Ingredient]
rachelMansfieldIngredients =
  [ pureIngredient (1 / 3) "cup" "+ 2 tablespoons coconut flour"
  , pureIngredientNoUnit 3 "eggs at room temperature"
  , pureIngredient 1 "tbsp" "maple syrup"
  , pureIngredientNoUnit 3 "medium/large ripe bananas mashed"
  , pureIngredient 1 "tbsp" "melted & cooled coconut oil"
  , pureIngredient 0.5 "tsp" "of baking powder"
  , pureIngredient 0.5 "cup" "of dark chocolate chips"
  , pureIngredient 0.5 "cup" "creamy nut butter"
  , pureIngredientNoQuantity "sprinkle" "of cinnamon"
  , pureIngredientNoQuantity "splash" "of vanilla extract"
  ]

foodNetworkIngredients :: [Ingredient]
foodNetworkIngredients =
  [ pureIngredientNoUnit 1 "(5 to 6 pound) roasting chicken"
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

sallysBakingIngredients :: [Ingredient]
sallysBakingIngredients =
  [ pureIngredient 0.5 "cup" "(115g; 1 stick) unsalted butter"
  , pureIngredient 6.0 "oz" "(170g) high quality semi-sweet chocolate*"
  , pureIngredient 0.25 "cup" "(31g) all-purpose flour (spoon & leveled)"
  , pureIngredient 0.5 "cup" "(60g) confectioners\8217 sugar"
  , pureIngredientNoUnit 2 "large egg yolks*"
  , pureIngredientNoUnit 2 "large eggs"
  , pureIngredientName "optional for topping: ice cream, raspberries, and/or chocolate syrup"
  , pureIngredient 0.125 "tsp" "salt"
  ]

cafeDelitesIngredients :: [Ingredient]
cafeDelitesIngredients =
  [ pureIngredient 14 "oz" "(400g) tomato puree (tomato sauce/Passata)"
  , pureIngredient 28 "oz" "(800g) boneless and skinless chicken thighs cut into bite-sized pieces"
  , pureIngredient 1 "tsp" "brown sugar"
  , pureIngredient 2 "tbsp" "butter"
  , pureIngredient 4 "tbsp" "Fresh cilantro or coriander to garnish"
  , pureIngredient 1.5 "tsp" "garam masala"
  , pureIngredient 2 "tsp" "garam masala"
  , pureIngredient 1.5 "tbsp" "garlic finely grated"
  , pureIngredient 1 "tbsp" "ginger"
  , pureIngredient 1.0 "tbsp" "ginger finely grated"
  , pureIngredient 1 "tsp" "ground coriander"
  , pureIngredient 1 "tsp" "ground cumin"
  , pureIngredient 1.5 "tsp" "ground cumin"
  , pureIngredient 1 "tsp" "ground red chili powder (adjust to your taste preference)"
  , pureIngredient 1 "tsp" "Kashmiri chili (optional for colour and flavour)"
  , pureIngredient 1 "tsp" "Kashmiri chili (or 1/2 teaspoon ground red chili powder)"
  , pureIngredient 1.5 "tbsp" "minced garlic"
  , pureIngredient 1.25 "cup" "of heavy or thickened cream (use evaporated milk for lower calories)"
  , pureIngredient 1 "tsp" "of salt"
  , pureIngredient 2 "tbsp" "of vegetable/canola oil"
  , pureIngredient 1 "cup" "plain yogurt"
  , pureIngredient 1 "tsp" "salt"
  , pureIngredientNoUnit 2 "small onions (or 1 large onion) finely diced"
  , pureIngredient 1 "tsp" "turmeric"
  , pureIngredient 1 "tsp" "turmeric powder"
  , pureIngredient 0.25 "cup" "water if needed"
  ]

eatingWellIngredients :: [Ingredient]
eatingWellIngredients =
  [ pureIngredientNoUnit 1 "(5-ounce) block feta cheese"
  , pureIngredient 2 "cup" "boiling water"
  , pureIngredient 1 "tsp" "dried dill"
  , pureIngredient 3 "tbsp" "extra-virgin olive oil"
  , pureIngredient 0.25 "tsp" "ground pepper"
  , pureIngredient 0.25 "tsp" "kosher salt"
  , pureIngredientNoUnit 2 "large cloves garlic, minced"
  , pureIngredient 8 "cup" "lightly packed baby spinach (about 5 ounces)"
  , pureIngredient 8 "oz" "penne or rotini"
  ]
