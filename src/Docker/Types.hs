{-# LANGUAGE OverloadedStrings #-}

module Docker.Types where

import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Default
import Data.Map as Map
import Control.Lens.Operators
import Network.Wreq

-- | ReaderT for passing around configuration
type DockerClient a = ReaderT DockerClientConfig IO a

-- | Client configuration
data DockerClientConfig = DockerClientConfig
    { apiBase :: String
    } deriving (Eq, Show)

-- | Default settings for client configuration
instance Default DockerClientConfig where
    def = DockerClientConfig {
            apiBase = "http://192.168.1.4:2375"
        }


