module Chez.Grater.TestEnv where

import Chez.Grater.Internal.Prelude

import Chez.Grater.Manager (createManager)
import Network.HTTP.Client (Manager)

data Env = Env
  { envManager :: Manager
  }

loadEnv :: IO Env
loadEnv = do
  manager <- createManager
  pure $ Env manager
