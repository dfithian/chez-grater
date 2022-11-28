module Chez.Grater.Scraper.Types where

import Chez.Grater.Internal.Prelude

import Data.Hashable (Hashable)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.HTML.Scalpel (Scraper)
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

-- |Wrapper for scraper metadata.
data ScrapeMetaWrapper
  = ScrapeMetaWrapperIngredient (ScrapeMeta ScrapedIngredient)
  | ScrapeMetaWrapperIngredientAndStep (ScrapeMeta ScrapedIngredient) (ScrapeMeta ScrapedStep)
  deriving (Eq, Ord, Show)

-- |Name of a scraper.
newtype ScrapeName = ScrapeName { unScrapeName :: Text }
  deriving (Eq, Ord, Show, Generic)

-- |Version of a scraper.
newtype ScrapeVersion = ScrapeVersion { unScrapeVersion :: Int }
  deriving (Eq, Ord, Show, Generic)

-- |Metadata for a scraper.
data ScrapeMeta a = ScrapeMeta
  { scrapeMetaName    :: ScrapeName
  , scrapeMetaVersion :: ScrapeVersion
  }
  deriving (Eq, Ord, Show, Generic)

-- |The `<title>` element of the HTML.
newtype ScrapedRecipeName = ScrapedRecipeName { unScrapedRecipeName :: Text }
  deriving (Eq, Show, Generic)

-- |Unparsed ingredient.
newtype ScrapedIngredient = ScrapedIngredient { unScrapedIngredient :: Text }
  deriving (Eq, Ord, Show)

newtype ScrapedStep = ScrapedStep { unScrapedStep :: Text }
  deriving (Eq, Ord, Show)

data IngredientScraper = IngredientScraper
  { ingredientScraperMeta :: ScrapeMeta ScrapedIngredient
  -- ^Metadata about the scraper.
  , ingredientScraperTest :: Scraper Text Bool
  -- ^A test on the HTML to see if this scraper should work.
  , ingredientScraperRun  :: Scraper Text [ScrapedIngredient]
  -- ^Run the scraper!
  }

data StepScraper = StepScraper
  { stepScraperMeta :: ScrapeMeta ScrapedStep
  -- ^Metadata about the scraper.
  , stepScraperTest :: Scraper Text Bool
  -- ^A test on the HTML to see if this scraper should work.
  , stepScraperRun  :: Scraper Text [ScrapedStep]
  -- ^Run the scraper!
  }

-- |Domain like `halfbakedharvest.com`.
newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

data Scrapers = Scrapers
  { scrapersIngredientBySite :: HashMap SiteName IngredientScraper
  , scrapersIngredients      :: [IngredientScraper]
  , scrapersStepBySite       :: HashMap SiteName StepScraper
  , scrapersSites            :: [StepScraper]
  }

data ScrapeError = ScrapeError Text
  deriving (Eq, Show)

instance Exception ScrapeError

title :: Scraper Text ScrapedRecipeName
title = ScrapedRecipeName . Text.strip <$> Scalpel.text "title"

inception :: ScrapeVersion
inception = ScrapeVersion 1

hasClassPrefix :: Text -> Scalpel.AttributePredicate
hasClassPrefix cls =
  Scalpel.match $ \case
    "class" -> \classes -> any (\cls' -> cls `Text.isPrefixOf` cls') . Text.split ((==) ' ') . Text.pack $ classes
    _ -> const False
