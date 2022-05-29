module Cheez.Scraper.Types where

import Cheez.Prelude

import Cheez.Types (RecipeName(..), Ingredient, Step)
import Data.Hashable (Hashable)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.HTML.Scalpel (Scraper)
import qualified Data.Text as Text
import qualified Text.HTML.Scalpel as Scalpel

data ScrapedInfo
  = ScrapedInfoIngredient (ScrapeInfo Ingredient)
  | ScrapedInfoIngredientStep (ScrapeInfo Ingredient) (ScrapeInfo Step)
  deriving (Eq, Ord, Show)

newtype ScrapeName = ScrapeName { unScrapeName :: Text }
  deriving (Eq, Ord, Show, Generic)

newtype ScrapeVersion = ScrapeVersion { unScrapeVersion :: Int }
  deriving (Eq, Ord, Show, Generic)

data ScrapeInfo a = ScrapeInfo
  { scrapeInfoName    :: ScrapeName
  , scrapeInfoVersion :: ScrapeVersion
  }
  deriving (Eq, Ord, Show, Generic)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show, Generic)

data IngredientScraper = IngredientScraper
  { ingredientScraperInfo :: ScrapeInfo Ingredient
  , ingredientScraperTest :: Scraper Text Bool
  , ingredientScraperRun  :: Scraper Text [UnparsedIngredient]
  }

data StepScraper = StepScraper
  { stepScraperInfo :: ScrapeInfo Step
  , stepScraperTest :: Scraper Text Bool
  , stepScraperRun  :: Scraper Text [UnparsedStep]
  }

newtype SiteName = SiteName { unSiteName :: Text }
  deriving (Eq, Ord, Show, IsString, Hashable)

data UnparsedIngredient = UnparsedIngredientRaw Text
  deriving (Eq, Ord, Show)

data UnparsedStep = UnparsedStepRaw Text
  deriving (Eq, Ord, Show)

data ScrapeError = ScrapeError Text
  deriving (Eq, Show)

instance Exception ScrapeError

title :: Scraper Text RecipeName
title = RecipeName . Text.strip <$> Scalpel.text "title"

inception :: ScrapeVersion
inception = ScrapeVersion 1
