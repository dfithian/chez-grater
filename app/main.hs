import Chez.Grater.Internal.Prelude

import Chez.Grater (scrapeAndParseUrl, scrapeUrl)
import Chez.Grater.Manager (createManager)
import Chez.Grater.Parser (mkIngredients, quantityP, runParser)
import Chez.Grater.Parser.Types (ParsedQuantity(..))
import Chez.Grater.Readable.Types (mkReadableIngredient, mkReadableQuantity, showReadableIngredient, showReadableQuantity)
import Chez.Grater.Scraper.Site (allScrapers)
import Chez.Grater.Scraper.Types (ScrapedIngredient(..), ScrapedRecipeName(..), ScrapedStep(..))
import Chez.Grater.Types (unIngredientName, Ingredient(..), IngredientName(..), Quantity(..), RecipeName(..), Step(..))
import Data.Aeson ((.=), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Network.URI (parseURI)
import Options.Applicative ((<**>))
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

data Output = OutputText | OutputJson
  deriving (Eq)

instance Show Output where
  show = \case
    OutputText -> "text"
    OutputJson -> "json"

data Opts = Opts
  { optsUrl :: String
  , optsNoParse :: Bool
  , optsScale :: Maybe Double
  , optsOutput :: Output
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser (Opt.info (parser <**> Opt.helper) (Opt.progDesc "Scrape a recipe from a URL"))
  where
    parser = Opts
      <$> Opt.strArgument
        ( Opt.metavar "URL"
        )
      <*> Opt.switch
        ( Opt.long "no-parse"
            <> Opt.help "Don't parse the recipe"
        )
      <*> Opt.option readScale
        ( Opt.short 's'
            <> Opt.long "scale"
            <> Opt.value Nothing
            <> Opt.showDefault
            <> Opt.help "Adjust ingredient quantities"
        )
      <*> Opt.option readOutput
        ( Opt.short 'o'
            <> Opt.long "output"
            <> Opt.value OutputText
            <> Opt.showDefault
            <> Opt.help "Output format for the recipe (text or json)"
        )
    readOutput = Opt.maybeReader $ \case
      "text" -> Just OutputText
      "json" -> Just OutputJson
      _ -> Nothing
    readScale = Opt.eitherReader $ \str ->
      case (runParser quantityP . Text.pack) str of
        Right (ParsedQuantity 1) -> Right Nothing
        Right (ParsedQuantity q) -> Right (Just q)
        Right _ -> Left "Expected numeric quantity"
        Left err -> Left err

scaleIngredientBy :: Double -> Ingredient -> Ingredient
scaleIngredientBy scale (Ingredient name quantity unit) =
  case quantity of
    Quantity q ->
      Ingredient name (Quantity (q*scale)) unit
    QuantityMissing ->
      Ingredient
        (IngredientName . (prefix <>) . unIngredientName $ name)
        quantity
        unit
  where
    prefix = case (showReadableQuantity . mkReadableQuantity . Quantity) scale of
      Just str -> CI.mk $ "(" <> str <> "x) "  -- FIXME: should be "×" but unicode 😵‍💫
      Nothing -> ""

showRecipe :: RecipeName -> [Ingredient] -> [Step] -> IO ()
showRecipe (RecipeName name) ingredients steps = do
  let width = length (Text.unpack name)
  putStrLn $ Text.unpack name
  putStrLn $ replicate width '-'
  traverse_ putStrLn $ fmap (Text.unpack . showReadableIngredient . mkReadableIngredient) ingredients
  putStrLn $ replicate width '-'
  traverse_ putStrLn $ fmap (("- " <>) . Text.unpack . unStep) steps

jsonRecipe :: RecipeName -> [Ingredient] -> [Step] -> IO ()
jsonRecipe name ingredients steps =
  putStrLn . LC8.unpack . encodePretty . object $
    [ "name" .= name
    , "ingredients" .= fmap mkReadableIngredient ingredients
    , "steps" .= fmap unStep steps
    ]

renderRecipe :: Output -> RecipeName -> [Ingredient] -> [Step] -> IO ()
renderRecipe = \case
  OutputText -> showRecipe
  OutputJson -> jsonRecipe

main :: IO ()
main = do
  Opts {..} <- parseArgs
  manager <- createManager
  url <- maybe (fail "Invalid URL") pure $ parseURI optsUrl
  case optsNoParse of
    True -> do
      (name, ingredients, steps, _) <- scrapeUrl allScrapers manager url
      renderRecipe optsOutput
        (RecipeName . unScrapedRecipeName $ name)
        (concatMap (mkIngredients . unScrapedIngredient) ingredients)
        (fmap (Step . unScrapedStep) steps)
    False -> do
      (name, ingredients, steps, _) <- scrapeAndParseUrl allScrapers manager url
      let scaledIngredients :: [Ingredient] = (maybe id (fmap . scaleIngredientBy) optsScale) ingredients
      renderRecipe optsOutput name scaledIngredients steps
