
module Docker.Conduit.Client where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)
import Network.URL


data DockerClientConfig = DockerClientConfig
    { clientManager :: Manager
    , clientHost    :: Host
    }


type DockerClient m a = ReaderT DockerClientConfig m a

runClient :: MonadResource m => DockerClientConfig -> DockerClient m a -> m a
runClient cfg = flip runReaderT cfg

run :: IO ()
run = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    let dockerHost = Host (HTTP False) ("192.168.1.2") (Just 2375)
    let cfg = DockerClientConfig manager dockerHost
    runClient cfg $ do
        pullImage "tu" >>= \s -> responseBody s $$+- CB.sinkFile "out"


