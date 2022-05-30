# Chez Grater

![chez-grater](https://github.com/dfithian/chez-grater/actions/workflows/workflow.yml/badge.svg)

Scrape and parse recipe blogs to skip the ads. Base server logic behind https://app.mo-nomz.com/.

## Usage

```haskell
import Prelude

import Chez.Grater (scrapeAndParseUrl)
import Chez.Grater.Manager (createManager)
import Control.Monad (fail)
import Network.URI (parseURI)

main :: IO ()
main = do
  uri <- maybe (fail "Invalid URI") pure (parseURI "https://www.halfbakedharvest.com/southern-butter-biscuits/")
  manager <- createManager
  (name, ingredients, steps, _) <- scrapeAndParseUrl manager uri
  putStrLn $ show (name, ingredients, steps)
```
