import Prelude

import Cheez.TestEnv (loadEnv)
import Test.Hspec (hspec)
import qualified Cheez.ConversionSpec
import qualified Cheez.ParserSpec
import qualified Cheez.ScrapeSpec

main :: IO ()
main = do
  env <- loadEnv
  hspec $ do
    Cheez.ConversionSpec.spec
    Cheez.ParserSpec.spec
    Cheez.ScrapeSpec.spec env
