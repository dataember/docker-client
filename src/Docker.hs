{-# LANGUAGE OverloadedStrings #-}

module Docker where

import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Docker.Types


-- | Run the client
runDockerClient :: MonadIO m => DockerClientConfig -> DockerClient a -> m a
runDockerClient config action = liftIO $ runReaderT action config



