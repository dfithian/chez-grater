module Cheez.Parser where

import Cheez.Prelude

import Cheez.Scraper.Types (UnparsedIngredient(..), UnparsedStep(..))
import Cheez.Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RawIngredient(..), RawQuantity(..)
  , RawUnit(..), Step(..), Unit(..), Ingredient, box, cup, gram, liter, milligram, milliliter, ounce
  , pinch, pound, splash, sprinkle, tablespoon, teaspoon, whole
  )
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Function (fix)
import Text.Read (readMaybe)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

unitAliasTable :: Map (CI Text) Unit
unitAliasTable = Map.fromList
  [ ("ounce", ounce)
  , ("ounces", ounce)
  , ("oz", ounce)
  , ("c", cup)
  , ("cup", cup)
  , ("cups", cup)
  , ("tablespoon", tablespoon)
  , ("tablespoons", tablespoon)
  , ("tbsp", tablespoon)
  , ("teaspoon", teaspoon)
  , ("teaspoons", teaspoon)
  , ("tsp", teaspoon)
  , ("pinch", pinch)
  , ("pinches", pinch)
  , ("box", box)
  , ("boxes", box)
  , ("pound", pound)
  , ("pounds", pound)
  , ("splash", splash)
  , ("splashes", splash)
  , ("sprinkle", sprinkle)
  , ("sprinkles", sprinkle)
  , ("whole", whole)

  , ("milliliter", milliliter)
  , ("millilitre", milliliter)
  , ("ml", milliliter)
  , ("liter", liter)
  , ("litre", liter)
  , ("l", liter)
  , ("milligram", milligram)
  , ("mg", milligram)
  , ("gram", gram)
  , ("g", gram)
  ]

quantityAliasTable :: Map (CI Text) Quantity
quantityAliasTable = Map.fromList
  [ ("half dozen", 6)
  , ("dozen", 12)
  , ("quarter", 0.25)
  , ("third", 1 / 3)
  , ("half", 0.5)
  , ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  , ("ten", 10)
  , ("eleven", 11)
  , ("twelve", 12)
  ]

scrubUnit :: RawUnit -> Unit
scrubUnit = \case
  RawUnit x -> Map.findWithDefault (Unit x) x unitAliasTable
  RawUnitMissing -> UnitMissing

scrubQuantity :: RawQuantity -> Quantity
scrubQuantity = \case
  RawQuantity q -> Quantity q
  RawQuantityWord w -> Map.findWithDefault QuantityMissing w quantityAliasTable
  RawQuantityMissing -> QuantityMissing

scrubIngredient :: RawIngredient -> Ingredient
scrubIngredient RawIngredient {..} = Ingredient
  { ingredientName = rawIngredientName
  , ingredientQuantity = scrubQuantity rawIngredientQuantity
  , ingredientUnit = scrubUnit rawIngredientUnit
  }

quantityP :: Atto.Parser RawQuantity
quantityP = quantityExpression <|> quantityWord <|> quantityMissing
  where
    isIgnoredC c = elem c ['Â']
    isQuantityC c = isDigit c || isSpace c || elem c ['/', '.', '-', '⁄', '¼', '½', '¾', '⅓', '⅔'] || isIgnoredC c
    quantityParser p = p . Text.filter (not . isIgnoredC) =<< Atto.takeWhile isQuantityC
    strictQuantityParser p = p . Text.strip . Text.filter (not . isIgnoredC) =<< Atto.takeWhile1 isQuantityC

    quantitySingle str = maybe (fail $ Text.unpack str <> " is not a single quantity") pure . readMaybe . Text.unpack . Text.filter (not . isSpace) $ str
    quantityUnicode = \case
      "¼" -> pure 0.25
      "½" -> pure 0.5
      "¾" -> pure 0.75
      "⅓" -> pure $ 1 / 3
      "⅔" -> pure $ 2 / 3
      str -> fail $ Text.unpack str <> " is not a unicode quantity"
    quantityDecimal str = case Text.split ((==) '.') str of
      [x, y] -> maybe (fail $ Text.unpack str <> " is not a decimal quantity") pure $ do
        x' <- fromInteger <$> readMaybe (Text.unpack x)
        y' <- fromInteger <$> readMaybe (Text.unpack y)
        pure $ x' + (y' / (fromIntegral $ 10 * Text.length y))
      _ -> fail $ Text.unpack str <> " is not a decimal quantity"
    quantityFraction str = case Text.split ((==) '/') $ Text.replace "⁄" "/" str of
      [x, y] -> maybe (fail $ Text.unpack str <> " is not a fractional quantity") pure $
        (/) <$> readMaybe (Text.unpack x) <*> readMaybe (Text.unpack y)
      _ -> fail $ Text.unpack str <> " is not a fractional quantity"

    quantityImproper = quantityParser $ \str -> case filter (not . Text.null) . mconcat . fmap (Text.split isSpace) . Text.split ((==) '-') $ str of
      [x, y] -> do
        x' <- quantitySimple x
        y' <- quantitySimple y
        pure $ if x' < y' then (x' + y') / 2 else x' + y'
      _ -> fail $ Text.unpack str <> " is not an improper quantity"

    quantitySimple str =
      quantitySingle str
        <|> quantityUnicode str
        <|> quantityDecimal str
        <|> quantityFraction str

    quantityExpression = RawQuantity <$> (strictQuantityParser quantitySimple <|> quantityImproper)
    quantityWord = RawQuantityWord . CI.mk <$> ((\str -> if CI.mk str `elem` Map.keys quantityAliasTable then pure str else fail $ Text.unpack str <> " is not a quantity") =<< spaced (Atto.takeWhile1 isAlpha))
    quantityMissing = quantityParser $ \str -> case Text.null str of
      True -> pure RawQuantityMissing
      False -> fail $ Text.unpack str <> " is a quantity, but thought it was missing"

spaced :: Atto.Parser a -> Atto.Parser a
spaced p = optional (void Atto.space) *> (p <* optional (void Atto.space))

unitP :: Atto.Parser RawUnit
unitP = unitWord <|> pure RawUnitMissing
  where
    isIgnoredC c = elem c ['.']
    isUnitC c = isAlpha c || isIgnoredC c
    unitWord = do
      unit <- CI.mk . Text.filter (not . isIgnoredC) <$> spaced (Atto.takeWhile1 isUnitC)
      case unit `elem` Map.keys unitAliasTable of
        True -> pure $ RawUnit unit
        False -> fail "No unit found"

nameP :: Atto.Parser IngredientName
nameP = IngredientName . CI.mk . Text.strip . Text.unwords . filter (not . Text.null) . fmap Text.strip . Text.words <$> Atto.takeText

ingredientP :: Atto.Parser RawIngredient
ingredientP = mk <$> ((,,) <$> quantityP <*> unitP <*> nameP)
  where
    mk (q, u, n) = RawIngredient n q u

sanitize :: Text -> Text
sanitize = replacements . Text.filter (not . isIgnoredC)
  where
    replacements str = foldr (uncurry Text.replace) str
      [ ("\194", " ")
      , ("\226\150\162", "")
      ]
    isIgnoredC c = elem c ['▢', '☐']

runParser :: Atto.Parser a -> Text -> Either String a
runParser parser x = Atto.parseOnly parser (Text.strip (sanitize x))

parseIngredients :: [UnparsedIngredient] -> Either Text [Ingredient]
parseIngredients xs = left (const "Failed to parse ingredients") . fmap (nubOrd . fmap scrubIngredient . catMaybes) . for xs $ \case
  UnparsedIngredientRaw raw | Text.null raw -> pure Nothing
  UnparsedIngredientRaw raw -> Just <$> runParser ingredientP raw

parseRawIngredients :: Text -> Either Text [Ingredient]
parseRawIngredients content = do
  either (const $ Left "Failed to parse ingredients") (pure . fmap scrubIngredient)
    . traverse (runParser ingredientP)
    . filter (not . Text.null)
    . Text.lines
    $ content

unparseRawIngredients :: Text -> [Ingredient]
unparseRawIngredients =
  fmap (\str -> Ingredient (IngredientName (CI.mk str)) QuantityMissing UnitMissing) . Text.lines

parseSteps :: [UnparsedStep] -> Either Text [Step]
parseSteps = \case
  [UnparsedStepRaw single] | "1." `Text.isPrefixOf` single -> flip fix (filter (not . Text.null) . Text.words . Text.drop 2 $ single, (1 :: Int), []) $ \f -> \case
    ([], _, parsed) -> Right $ reverse parsed
    (toParse, ordinal, parsed) ->
      let nextOrdinal = tshow (ordinal + 1) <> "."
          (next, rest) = span (not . Text.isSuffixOf nextOrdinal) toParse
      in case rest of
        x:xs -> case Text.stripSuffix nextOrdinal x of
          Just y -> f (xs, ordinal + 1, (Step (Text.unwords (next <> [y]))):parsed)
          Nothing -> f (xs, ordinal + 1, (Step (Text.unwords next)):parsed)
        [] -> f ([], ordinal + 1, (Step (Text.unwords next)):parsed)
  [UnparsedStepRaw single] -> Right $ fmap (Step . Text.unwords . filter (not . Text.null) . Text.words) . filter (not . Text.null) . fmap Text.strip . Text.lines $ single
  xs -> Right $ fmap (\(UnparsedStepRaw step) -> Step . Text.unwords . filter (not . Text.null) . Text.words $ step) xs
