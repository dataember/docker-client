{-# LANGUAGE
    FlexibleContexts
    , OverloadedStrings
    #-}

module Docker.Conduit.Info where

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

import Docker.Conduit.Types

-- | Low level info on the docker daemon itself.
info :: (MonadReader DockerClientConfig m, MonadResource m)
    => m (Response (ResumableSource m BS.ByteString))
info = do
    config <- ask
    let host= clientHost config
    let manager = clientManager config
    initReq <- liftIO $ parseUrl $ (exportHost host) ++ "/info"
    http initReq{ method = "GET"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                } manager
