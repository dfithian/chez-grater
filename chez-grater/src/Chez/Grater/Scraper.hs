module Chez.Grater.Scraper where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Scraper.Types
  ( IngredientScraper(..), ScrapeError(..), ScrapeMetaWrapper(..), ScrapedRecipeName(..)
  , Scrapers(..), SiteName(..), StepScraper(..), ScrapedIngredient, ScrapedStep, title
  )
import Network.HTTP.Client (Manager)
import Network.URI (URI, uriAuthority, uriRegName)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

-- |Scrape a recipe. This is the low-level API, used by 'Chez.Grater'.
scrape
  :: (ScrapedRecipeName -> a)
  -> ([ScrapedIngredient] -> Either Text [b])
  -> ([ScrapedStep] -> Either Text [c])
  -> Scrapers -> Manager -> URI -> IO (a, [b], [c], ScrapeMetaWrapper)
scrape mkName runIngredientParser runStepParser scrapers manager uri = do
  let cfg = Scalpel.Config Scalpel.defaultDecoder (Just manager)
  tags <- Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . Text.replace "www." "" . Text.pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (ScrapedRecipeName "Untitled") $ Scalpel.scrape title tags

      runScraper :: forall a b. ([a] -> Either Text [b]) -> Scalpel.Scraper Text [a] -> Either Text [b]
      runScraper parser scraper = parser =<< maybe (Left "Nothing scraped") pure (Scalpel.scrape scraper tags)

      goIngredient IngredientScraper {..} = case Scalpel.scrape ingredientScraperTest tags of
        Just True -> (,ingredientScraperMeta) <$> either (const Nothing) Just (runScraper runIngredientParser ingredientScraperRun)
        _ -> Nothing
      goStep StepScraper {..} = case Scalpel.scrape stepScraperTest tags of
        Just True -> (,stepScraperMeta) <$> either (const Nothing) Just (runScraper runStepParser stepScraperRun)
        _ -> Nothing

  (ingredients, ingredientMeta) <- case flip HashMap.lookup (scrapersIngredientBySite scrapers) =<< domainMay of
    Just IngredientScraper {..} -> either (throwIO . ScrapeError) (pure . (,ingredientScraperMeta)) $ runScraper runIngredientParser ingredientScraperRun
    Nothing -> maybe (throwIO $ ScrapeError "Failed to scrape URL from defaults") pure
     . lastMay
     . sortOn (length . fst)
     . mapMaybe goIngredient
     . scrapersIngredients
     $ scrapers
  stepsMay <- case flip HashMap.lookup (scrapersStepBySite scrapers) =<< domainMay of
    Just StepScraper {..} -> pure . fmap (,stepScraperMeta) . either (const Nothing) Just . runScraper runStepParser $ stepScraperRun
    Nothing -> pure
      . lastMay
      . sortOn (length . fst)
      . mapMaybe goStep
      . scrapersSites
      $ scrapers
  case stepsMay of
    Just (steps, stepMeta) | not (null steps) -> pure (mkName name, ingredients, steps, ScrapeMetaWrapperIngredientAndStep ingredientMeta stepMeta)
    _ -> pure (mkName name, ingredients, [], ScrapeMetaWrapperIngredient ingredientMeta)
