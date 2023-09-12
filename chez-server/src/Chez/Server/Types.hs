module Chez.Server.Types where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Internal.Json (jsonOptions)
import Chez.Grater.Readable.Types (ReadableIngredient, ReadableStep)
import Chez.Grater.Types (Ingredient(..), RecipeName, Step)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)

newtype RecipeLink = RecipeLink { unRecipeLink :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

data ScrapedRecipe = ScrapedRecipe
  { scrapedRecipeName        :: RecipeName
  , scrapedRecipeIngredients :: [Ingredient]
  , scrapedRecipeSteps       :: [Step]
  } deriving (Eq, Show)

data ParseBlobRequest = ParseBlobRequest
  { parseBlobRequestContent :: Text
  }
  deriving (Eq, Ord, Show)

data ParseLinkRequest = ParseLinkRequest
  { parseLinkRequestLink :: RecipeLink
  }
  deriving (Eq, Ord, Show)

data ParseBlobResponse = ParseBlobResponse
  { parseBlobResponseIngredients :: [ReadableIngredient]
  }
  deriving (Eq, Ord, Show)

data ParseLinkResponse = ParseLinkResponse
  { parseLinkResponseName        :: RecipeName
  , parseLinkResponseIngredients :: [ReadableIngredient]
  , parseLinkResponseSteps       :: [ReadableStep]
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "parseBlobRequest") ''ParseBlobRequest
deriveJSON (jsonOptions "parseLinkRequest") ''ParseLinkRequest
deriveJSON (jsonOptions "parseBlobResponse") ''ParseBlobResponse
deriveJSON (jsonOptions "parseLinkResponse") ''ParseLinkResponse
