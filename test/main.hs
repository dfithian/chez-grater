import Prelude

import Chez.Grater.TestEnv (loadEnv)
import Test.Hspec (hspec)
import qualified Chez.Grater.ConversionSpec
import qualified Chez.Grater.ParserSpec
import qualified Chez.Grater.ScrapeSpec

main :: IO ()
main = do
  env <- loadEnv
  hspec $ do
    Chez.Grater.ConversionSpec.spec
    Chez.Grater.ParserSpec.spec
    Chez.Grater.ScrapeSpec.spec env
