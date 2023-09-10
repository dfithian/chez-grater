import Chez.Grater.Internal.Prelude

import Test.Hspec (hspec)

import qualified Chez.Server.ConversionSpec as ConversionSpec

main :: IO ()
main = do
  hspec $ do
    ConversionSpec.spec
