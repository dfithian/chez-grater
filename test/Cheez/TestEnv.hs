module Cheez.TestEnv where

import Cheez.Prelude

import Network.HTTP.Client (Manager, managerModifyRequest, requestHeaders)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)

data Env = Env
  { envManager :: Manager
  }

createManager :: IO Manager
createManager =
  newTlsManagerWith tlsManagerSettings
    { managerModifyRequest = \req -> do
        pure req
          { requestHeaders = [(hUserAgent, "Simulated")] <> requestHeaders req
          }
    }

loadEnv :: IO Env
loadEnv = do
  manager <- createManager
  pure $ Env manager
