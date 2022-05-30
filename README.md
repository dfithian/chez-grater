# Chez Grater

![chez-grater](https://github.com/dfithian/chez-grater/actions/workflows/workflow.yml/badge.svg)

Scrape and parse recipe blogs to skip the ads. Base server logic behind https://app.mo-nomz.com/.

## Usage

```haskell
import Prelude

import Chez.Grater.Manager (createManager)
import Chez.Grater.Scrape (scrapeAndParseUrl)
import Control.Monad (fail)

main :: IO ()
main = do
  uri <- maybe (fail "Invalid URI") pure (parseURI "https://www.halfbakedharvest.com/southern-butter-biscuits/")
  manager <- createManager
  (name, ingredients, steps, _) <- fst <$> scrapeAndParseUrl manager uri
  putStrLn $ show (name, ingredients, steps)
```
