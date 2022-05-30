module Chez.Grater.Scraper where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Scraper.Site
  ( allIngredientScrapers, allStepScrapers, ingredientScrapers, stepScrapers
  )
import Chez.Grater.Scraper.Types
  ( IngredientScraper(..), ScrapeError(..), ScrapeMetaWrapper(..), ScrapedRecipeName(..)
  , SiteName(..), StepScraper(..), ScrapedIngredient, ScrapedStep, title
  )
import Network.HTTP.Client (Manager)
import Network.URI (URI, uriAuthority, uriRegName)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

scrape
  :: (ScrapedRecipeName -> a)
  -> ([ScrapedIngredient] -> Either Text [b])
  -> ([ScrapedStep] -> Either Text [c])
  -> Manager -> URI -> IO (a, [b], [c], ScrapeMetaWrapper)
scrape mkName runIngredientParser runStepParser manager uri = do
  let cfg = Scalpel.Config Scalpel.defaultDecoder (Just manager)
  tags <- Scalpel.fetchTagsWithConfig cfg (show uri)
  let domainMay = SiteName . Text.replace "www." "" . Text.pack . uriRegName <$> uriAuthority uri
      name = fromMaybe (ScrapedRecipeName "Untitled") $ Scalpel.scrape title tags

      runScraper :: forall a b. ([a] -> Either Text [b]) -> Scalpel.Scraper Text [a] -> Maybe [b]
      runScraper parser scraper = either (const Nothing) Just . parser =<< Scalpel.scrape scraper tags

      goIngredient IngredientScraper {..} = case Scalpel.scrape ingredientScraperTest tags of
        Just True -> (,ingredientScraperMeta) <$> runScraper runIngredientParser ingredientScraperRun
        _ -> Nothing
      goStep StepScraper {..} = case Scalpel.scrape stepScraperTest tags of
        Just True -> (,stepScraperMeta) <$> runScraper runStepParser stepScraperRun
        _ -> Nothing

  (ingredients, ingredientMeta) <- case flip HashMap.lookup ingredientScrapers =<< domainMay of
    Just IngredientScraper {..} -> maybe (throwIO $ ScrapeError "Failed to scrape known URL") (pure . (,ingredientScraperMeta)) $
      runScraper runIngredientParser ingredientScraperRun
    Nothing -> maybe (throwIO $ ScrapeError "Failed to scrape URL from defaults") pure
     . lastMay
     . sortOn (length . fst)
     . mapMaybe goIngredient
     $ allIngredientScrapers
  stepsMay <- case flip HashMap.lookup stepScrapers =<< domainMay of
    Just StepScraper {..} -> pure . fmap (,stepScraperMeta) . runScraper runStepParser $ stepScraperRun
    Nothing -> pure
      . lastMay
      . sortOn (length . fst)
      . mapMaybe goStep
      $ allStepScrapers
  case stepsMay of
    Just (steps, stepMeta) | not (null steps) -> pure (mkName name, ingredients, steps, ScrapeMetaWrapperIngredientAndStep ingredientMeta stepMeta)
    _ -> pure (mkName name, ingredients, [], ScrapeMetaWrapperIngredient ingredientMeta)
