module Chez.Server.Context where

import Chez.Grater.Internal.Prelude

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, catch, throwM)
import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
  ( Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLogger, logError, runStdoutLoggingT
  )
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.ByteString (ByteString)
import Network.HTTP.Client (Manager, managerModifyRequest, requestHeaders)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Servant.Server (Handler(Handler), ServerError)

type ChezM m = (MonadCatch m, MonadError ServerError m, MonadIO m, MonadLogger m, MonadReader ChezContext m)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type ChezServer = ExceptT ServerError (ReaderT ChezContext (LoggingT IO))

data ChezContext = ChezContext
  { chezManager :: Manager
  }

logErrors :: (MonadCatch m, MonadIO m) => m a -> m a
logErrors ma = do
  ma `catch` \(se :: SomeException) -> do
    runStdoutLoggingT ($logError (tshow se))
    throwM se

runChezServer :: ChezContext -> ChezServer a -> Handler a
runChezServer ctx ma = Handler (mapExceptT (\ma' -> runStdoutLoggingT (runReaderT ma' ctx)) ma)

userAgent :: ByteString
userAgent = "Simulated"

createManager :: IO Manager
createManager =
  newTlsManagerWith tlsManagerSettings
    { managerModifyRequest = \req -> do
        pure req
          { requestHeaders = [(hUserAgent, userAgent)] <> requestHeaders req
          }
    }

chezContext :: IO ChezContext
chezContext = ChezContext
  <$> createManager
