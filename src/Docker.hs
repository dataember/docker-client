{-# LANGUAGE OverloadedStrings #-}

module Docker where

import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Default
import Data.Map as Map
import Data.Text as T
import Control.Lens.Operators
import Network.Wreq

-- | ReaderT for passing around configuration
type DockerClient a = ReaderT DockerClientConfig IO a

-- | Client configuration
data DockerClientConfig = DockerClientConfig
    { apiBase :: T.Text
    } deriving (Eq, Show)

-- | Default settings for client configuration
instance Default DockerClientConfig where
    def = DockerClientConfig {
            apiBase = "http://192.168.1.4:2375"
        }

-- | Run the client
runDockerClient :: MonadIO m => DockerClientConfig -> DockerClient a -> m a
runDockerClient config action = liftIO $ runReaderT action config


-- * API Calls

-- | Retrieve info on all running containers.
listRunningContainers :: DockerClient [Map.Map T.Text Value]
listRunningContainers = do
    cfg <- ask
    r   <- getContainersJson (apiBase cfg)
    return $ r ^. responseBody
  where
    getContainersJson :: T.Text  -> DockerClient (Response [Map.Map T.Text Value])
    getContainersJson url = lift $
        asJSON =<< get (T.unpack url ++ "/containers/json")

