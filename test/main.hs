import Prelude

import Chez.Grater.TestEnv (loadEnv)
import Test.Hspec (hspec)
import qualified Chez.Grater.ParserSpec
import qualified Chez.GraterSpec

main :: IO ()
main = do
  env <- loadEnv
  hspec $ do
    Chez.Grater.ParserSpec.spec
    Chez.GraterSpec.spec env