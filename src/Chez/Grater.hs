module Chez.Grater where

import Chez.Grater.Prelude

import Chez.Grater.Parser (parseIngredients, parseSteps)
import Chez.Grater.Scraper.Site
  ( allIngredientScrapers, allStepScrapers, ingredientScrapers, stepScrapers
  )
import Chez.Grater.Scraper.Types
  ( IngredientScraper(..), ScrapeError(..), ScrapeInfo(..), ScrapedInfo(..), ScrapedRecipe(..)
  , SiteName(..), StepScraper(..), title
  )
import Chez.Grater.Types (RecipeName(..))
import Network.HTTP.Client (Manager)
import Network.URI (URI, uriAuthority, uriRegName)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

scrapeUrl :: Manager -> URI -> IO (ScrapedRecipe, ScrapedInfo)
scrapeUrl manager uri = do
  let cfg = Scalpel.Config Scalpel.defaultDecoder (Just manager)
  tags <- Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . Text.replace "www." "" . Text.pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (RecipeName "Untitled") $ Scalpel.scrape title tags
      runIngredientParser rawIngredients =
        parseIngredients rawIngredients >>= \case
          [] -> Left "No ingredients found"
          xs -> Right xs
      runStepParser rawSteps =
        parseSteps rawSteps >>= \case
          [] -> Left "No steps found"
          xs -> Right xs

      runScraper :: forall a b. ([a] -> Either Text [b]) -> Scalpel.Scraper Text [a] -> Maybe [b]
      runScraper parser scraper = either (const Nothing) Just . parser =<< Scalpel.scrape scraper tags

      goIngredient IngredientScraper {..} = case Scalpel.scrape ingredientScraperTest tags of
        Just True -> (,ingredientScraperInfo) <$> runScraper runIngredientParser ingredientScraperRun
        _ -> Nothing
      goStep StepScraper {..} = case Scalpel.scrape stepScraperTest tags of
        Just True -> (,stepScraperInfo) <$> runScraper runStepParser stepScraperRun
        _ -> Nothing
  (ingredients, ingredientInfo) <- case flip HashMap.lookup ingredientScrapers =<< domainMay of
    Just IngredientScraper {..} -> maybe (throwIO $ ScrapeError "Failed to scrape known URL") (pure . (,ingredientScraperInfo)) $
      runScraper runIngredientParser ingredientScraperRun
    Nothing -> maybe (throwIO $ ScrapeError "Failed to scrape URL from defaults") pure
     . lastMay
     . sortOn (length . fst)
     . mapMaybe goIngredient
     $ allIngredientScrapers
  stepsMay <- case flip HashMap.lookup stepScrapers =<< domainMay of
    Just StepScraper {..} -> pure . fmap (,stepScraperInfo) . runScraper runStepParser $ stepScraperRun
    Nothing -> pure
      . lastMay
      . sortOn (length . fst)
      . mapMaybe goStep
      $ allStepScrapers
  case stepsMay of
    Just (steps, stepInfo) | not (null steps) -> pure (ScrapedRecipe name ingredients steps, ScrapedInfoIngredientStep ingredientInfo stepInfo)
    _ -> do
      pure (ScrapedRecipe name ingredients [], ScrapedInfoIngredient ingredientInfo)

isInvalidScraper :: ScrapedInfo -> Bool
isInvalidScraper scrapeInfo =
  let matchesIngredientVersion info =
        (==) (Just (scrapeInfoVersion info))
          . fmap (scrapeInfoVersion . ingredientScraperInfo)
          . find ((==) (scrapeInfoName info) . scrapeInfoName . ingredientScraperInfo)
          $ allIngredientScrapers
      matchesStepVersion info =
        (==) (Just (scrapeInfoVersion info))
          . fmap (scrapeInfoVersion . stepScraperInfo)
          . find ((==) (scrapeInfoName info) . scrapeInfoName . stepScraperInfo)
          $ allStepScrapers
  in case scrapeInfo of
    ScrapedInfoIngredient ingredient -> not (matchesIngredientVersion ingredient)
    ScrapedInfoIngredientStep ingredient step -> not (matchesIngredientVersion ingredient) || not (matchesStepVersion step)
