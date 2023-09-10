module Chez.Grater where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Parser (parseScrapedIngredients, parseScrapedSteps)
import Chez.Grater.Scraper (scrape)
import Chez.Grater.Scraper.Types
  ( ScrapedRecipeName(..), ScrapeMetaWrapper, ScrapedIngredient, ScrapedStep, Scrapers
  )
import Chez.Grater.Types (RecipeName(..), Ingredient, Step)
import Network.HTTP.Client (Manager)
import Network.URI (URI)

-- |Scrape a URL without parsing it.
scrapeUrl :: Scrapers -> Manager -> URI -> IO (ScrapedRecipeName, [ScrapedIngredient], [ScrapedStep], ScrapeMetaWrapper)
scrapeUrl = scrape id Right Right

-- |Scrape a URL and also parse it.
scrapeAndParseUrl :: Scrapers -> Manager -> URI -> IO (RecipeName, [Ingredient], [Step], ScrapeMetaWrapper)
scrapeAndParseUrl = scrape
  (RecipeName . unScrapedRecipeName)
  parseScrapedIngredients
  parseScrapedSteps
