module Chez.Grater.Manager where

import Chez.Grater.Internal.Prelude

import Network.HTTP.Client (Manager, managerModifyRequest, requestHeaders)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)

-- |Creates a manager with a user agent, since some recipe blogs require that.
createManager :: IO Manager
createManager =
  newTlsManagerWith tlsManagerSettings
    { managerModifyRequest = \req -> do
        pure req
          { requestHeaders = [(hUserAgent, "Simulated")] <> requestHeaders req
          }
    }
