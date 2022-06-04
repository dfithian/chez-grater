module Chez.Grater.ParsedIngredients where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Types (Ingredient(..), IngredientName(..), Quantity(..), Step(..), Unit(..))
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

allRecipesSteps :: [Step]
allRecipesSteps =
  [ Step "Preheat oven to 425 degrees F (220 degrees C.)"
  , Step "In a saucepan, combine chicken, carrots, peas, and celery. Add water to cover and boil for 15 minutes. Remove from heat, drain and set aside."
  , Step "In the saucepan over medium heat, cook onions in butter until soft and translucent. Stir in flour, salt, pepper, and celery seed. Slowly stir in chicken broth and milk. Simmer over medium-low heat until thick. Remove from heat and set aside."
  , Step "Place the chicken mixture in bottom pie crust. Pour hot liquid mixture over. Cover with top crust, seal edges, and cut away excess dough. Make several small slits in the top to allow steam to escape."
  , Step "Bake in the preheated oven for 30 to 35 minutes, or until pastry is golden brown and filling is bubbly. Cool for 10 minutes before serving."
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

foodSteps :: [Step]
foodSteps =
  [ Step "Heat the oil in a 4 quart soup pot over medium heat."
  , Step "Add the onion, garlic, carrots, celery and thyme and cook until the vegetables are tender, about 10 minutes."
  , Step "Add the chicken broth, tomatoes and bay leaf."
  , Step "Drain the beans and roughly mash 1/2 a can of beans. Add the beans and mashed beans to the pot."
  , Step "Squeeze the water from the thawed spinach and add to the pot."
  , Step "Season with salt and pepper and simmer for at least 10 minutes and up to 30 minutes."
  , Step "Add the macaroni and cook until tender according to package directions. (About 10 minutes.)."
  , Step "Serve sprinkled with grated parmesan and/or pecorino romano."
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

pillsburySteps :: [Step]
pillsburySteps =
  [ Step "1 Heat oven to 425\176F. Prepare pie crusts as directed on box for Two-Crust Pie using 9-inch glass pie pan."
  , Step "2 In 2-quart saucepan, melt butter over medium heat. Add onion; cook 2 minutes, stirring frequently, until tender. Stir in flour, salt and pepper until well blended. Gradually stir in broth and milk, cooking and stirring until bubbly and thickened."
  , Step "3 Stir in chicken and mixed vegetables. Remove from heat. Spoon chicken mixture into crust-lined pan. Top with second crust; seal edge and flute. Cut slits in several places in top crust."
  , Step "4 Bake 30 to 40 minutes or until crust is golden brown. During last 15 to 20 minutes of baking, cover crust edge with strips of foil to prevent excessive browning. Let stand 5 minutes before serving."
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

bettyCrockerSteps :: [Step]
bettyCrockerSteps =
  [ Step "1 Heat oven to 350\176F. Spray bottom only of 13x9-inch pan with cooking spray."
  , Step "2 Make brownie batter as directed on box for cakelike brownies. Stir in 1 cup chopped cookies. Spread in pan. Bake 22 to 25 minutes or until toothpick inserted 2 inches from side of pan comes out almost clean. Cool completely, about 1 hour."
  , Step "3 In medium bowl, beat frosting and whipped topping with spoon until well blended. Spread over top of brownie. Sprinkle marshmallows over frosting layer. Top with remaining chopped cookies."
  , Step "4 In small microwavable bowl, microwave chocolate chips and whipping cream uncovered on High 45 seconds; stir. Continue to microwave in 15-second increments until chips are melted. Stir until smooth. Drizzle over top. Refrigerate about 1 hour or until chocolate is set."
  , Step "5 Cut into 6 rows by 4 rows. Store loosely covered in refrigerator."
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

tasteOfHomeSteps :: [Step]
tasteOfHomeSteps =
  [ Step "Preheat oven to 425\194\176. Place potatoes and carrots in a large saucepan; add water to cover. Bring to a boil. Reduce heat; cook, covered, 8-10 minutes or until crisp-tender; drain."
  , Step "In a large skillet, heat butter over medium-high heat. Add onion; cook and stir until tender. Stir in flour and seasonings until blended. Gradually stir in broth and milk. Bring to a boil, stirring constantly; cook and stir 2 minutes or until thickened. Stir in chicken, peas, corn and potato-carrot mixture; remove from heat."
  , Step "Unroll a pie crust into each of two 9-in. pie plates; trim crusts even with rims of plates. Add chicken mixture. Unroll remaining crusts; place over filling. Trim, seal and flute edges. Cut slits in tops."
  , Step "Bake 35-40 minutes or until crust is lightly browned. Let stand 15 minutes before cutting."
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

rachelMansfieldSteps :: [Step]
rachelMansfieldSteps =
  [ Step "Preheat oven to 350 and line or spray a loaf pan with parchment paper (I used 8.5 x 4.5 x 2.5)"
  , Step "Place wet ingredients in a medium mixing bowl and mix with kitchen aid"
  , Step "Once mixed well, add dry ingredients to wet ingredients and continue to mix with kitchen aid"
  , Step "Fold in dark chocolate chips once everything is mixed well (I also saved some to sprinkle on top)"
  , Step "Bake in oven for 35-45 minutes (or until ends are golden)"
  , Step "Finally, enjoy with your toppings of choice or deliciously as is"
  , Step "You can keep in fridge for about 7 days or freeze for longer!"
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

foodNetworkSteps :: [Step]
foodNetworkSteps =
  [ Step "Preheat the oven to 425 degrees F."
  , Step "Remove the chicken giblets. Rinse the chicken inside and out. Remove any excess fat and leftover pin feathers and pat the outside dry. Liberally salt and pepper the inside of the chicken. Stuff the cavity with the bunch of thyme, both halves of lemon, and all the garlic. Brush the outside of the chicken with the butter and sprinkle again with salt and pepper. Tie the legs together with kitchen string and tuck the wing tips under the body of the chicken. Place the onions, carrots, and fennel in a roasting pan. Toss with salt, pepper, 20 sprigs of thyme, and olive oil. Spread around the bottom of the roasting pan and place the chicken on top."
  , Step "Roast the chicken for 1 1/2 hours, or until the juices run clear when you cut between a leg and thigh. Remove the chicken and vegetables to a platter and cover with aluminum foil for about 20 minutes. Slice the chicken onto a platter and serve it with the vegetables."
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

sallysBakingSteps :: [Step]
sallysBakingSteps =
  [ Step "Spray four 6 ounce ramekin with nonstick cooking spray and dust with cocoa powder. This ensures the cakes will seamlessly come out of the ramekins when inverted onto a plate in step 7. *Or spray half of a 12-count muffin pan and dust with cocoa powder. If baking in a muffin pan, the recipe will yield 6 cakes."
  , Step "Preheat oven to 425\194\176F (218\194\176C)."
  , Step "Coarsely chop the chocolate. Place butter into a medium heat-proof bowl, then add chopped chocolate on top. Microwave on high in 10 second increments, stirring after each until completely smooth. Set aside."
  , Step "Whisk the flour, confectioners\8217 sugar, and salt together in a small bowl. Whisk the eggs and egg yolks together until combined in another small bowl. Pour the flour mixture and eggs into the bowl of chocolate. Slowly stir everything together using a rubber spatula or wooden spoon. If there are any lumps, gently use your whisk to rid them. The batter\194 will be slightly thick."
  , Step "Spoon chocolate batter evenly into each prepared ramekin or muffin cup."
  , Step "Place ramekins onto a baking sheet and bake for 12-14 minutes until the sides appear solid and firm\8211 the tops will still look soft. *If baking in a muffin pan, the cakes only take about 8-10 minutes."
  , Step "Allow to cool for 1 minute, then cover each with an inverted plate and turn over. Use an oven mitt because those ramekins are hot! The cakes should release easily from the ramekin. *If you used a muffin pan, use a spoon to release the cakes from the pan and place each upside down on plates."
  , Step "Add toppings. Serve immediately."
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

cafeDelitesSteps :: [Step]
cafeDelitesSteps =
  [ Step "In a bowl, combine chicken with all of the ingredients for the chicken marinade; let marinate for 10 minutes to an hour (or overnight if time allows)."
  , Step "Heat oil in a large skillet or pot over medium-high heat. When sizzling, add chicken pieces in batches of two or three, making sure not to crowd the pan. Fry until browned for only 3 minutes on each side. Set aside and keep warm. (You will finish cooking the chicken in the sauce.)"
  , Step "Melt the butter in the same pan. Fry the onions until soft (about 3 minutes) while scraping up any browned bits stuck on the bottom of the pan."
  , Step "Add garlic and ginger and saut\195\169 for 1 minute until fragrant, then add garam masala, cumin, turmeric and coriander. Fry for about 20 seconds until fragrant, while stirring occasionally."
  , Step "Pour in the tomato puree, chili powders and salt. Let simmer for about 10-15 minutes, stirring occasionally until sauce thickens and becomes a deep brown red colour."
  , Step "Stir the cream and sugar through the sauce. Add the chicken and its juices back into the pan and cook for an additional 8-10 minutes until chicken is cooked through and the sauce is thick and bubbling. Pour in the water to thin out the sauce, if needed."
  , Step "Garnish with cilantro (coriander) and serve with hot\194 garlic butter rice\194 and fresh homemade\194 Naan bread!"
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

eatingWellSteps :: [Step]
eatingWellSteps =
  [ Step "Preheat oven to 400°F."
  , Step "Place feta in the center of a 9-by-13-inch baking dish. Bake until softened and starting to brown, about 15 minutes."
  , Step "Meanwhile, combine spinach, oil, garlic, dill, salt and pepper in a large bowl. Use your hands to massage the spinach until it's reduced in volume by half. Stir in pasta."
  , Step "After the feta has baked for 15 minutes, add the spinach and pasta mixture to the baking dish. Pour boiling water over the mixture and gently stir. Cover with foil and bake until the pasta is tender, about 18 minutes. Remove from the oven and stir. Cover and let stand for at least 3 minutes before serving."
  ]
