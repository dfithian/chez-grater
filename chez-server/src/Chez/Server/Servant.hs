module Chez.Server.Servant where

import Chez.Server.Types (ParseBlobRequest, ParseBlobResponse, ParseLinkRequest, ParseLinkResponse)
import Data.Proxy (Proxy(Proxy))
import Servant.API ((:<|>), (:>), JSON, Post, ReqBody)

chezApi :: Proxy ChezApi
chezApi = Proxy

type ChezApi =
  "api" :> "v1" :> "blob" :> ReqBody '[JSON] ParseBlobRequest :> Post '[JSON] ParseBlobResponse
    :<|> "api" :> "v1" :> "link" :> ReqBody '[JSON] ParseLinkRequest :> Post '[JSON] ParseLinkResponse
