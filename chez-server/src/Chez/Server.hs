module Chez.Server where

import Chez.Grater.Internal.Prelude

import Chez.Grater (scrapeAndParseUrl)
import Chez.Grater.Parser (mkIngredients, parseRawIngredients)
import Chez.Grater.Readable.Types (mkReadableIngredient, mkReadableStep)
import Chez.Grater.Scraper.Site (allScrapers)
import Chez.Server.Context (ChezContext(..), ChezM, logErrors)
import Chez.Server.Types
  ( ParseBlobRequest(..), ParseBlobResponse(..), ParseLinkRequest(..), ParseLinkResponse(..)
  , RecipeLink(..), ScrapedRecipe(..)
  )
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logError)
import Control.Monad.Reader (ask)
import Network.URI (parseURI)
import Servant.Server (err400, errReasonPhrase)
import qualified Data.Text as Text

scrapeUrl :: ChezM m => RecipeLink -> m ScrapedRecipe
scrapeUrl link = do
  ChezContext {..} <- ask
  let scrape = do
        uri <- maybe (throwM err400 { errReasonPhrase = "Invalid link" }) pure $ parseURI (Text.unpack $ unRecipeLink link)
        (name, ingredients, steps, meta) <- scrapeAndParseUrl allScrapers chezManager uri
        pure (ScrapedRecipe name ingredients steps, meta)
  fst <$> logErrors (liftIO scrape)

postParseBlob :: ChezM m => ParseBlobRequest -> m ParseBlobResponse
postParseBlob ParseBlobRequest {..} = do
  ingredients <- case parseRawIngredients parseBlobRequestContent of
    Left e -> do
      $logError e
      pure $ mkIngredients parseBlobRequestContent
    Right is -> pure is
  pure ParseBlobResponse
    { parseBlobResponseIngredients = mkReadableIngredient <$> ingredients
    }

postParseLink :: ChezM m => ParseLinkRequest -> m ParseLinkResponse
postParseLink ParseLinkRequest {..} = do
  ScrapedRecipe {..} <- scrapeUrl parseLinkRequestLink
  pure ParseLinkResponse
    { parseLinkResponseName = scrapedRecipeName
    , parseLinkResponseIngredients = mkReadableIngredient <$> scrapedRecipeIngredients
    , parseLinkResponseSteps = mkReadableStep <$> scrapedRecipeSteps
    }
