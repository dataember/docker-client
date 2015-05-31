{-# LANGUAGE
    AllowAmbiguousTypes
    , DataKinds
    , FlexibleContexts
    , FlexibleInstances
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}

module Docker.Conduit.Image where

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

-- | Pull an image of the given name and tag from the specifiedi
-- repo and registry.
--
-- FIXME : The parameters here should be typed, not just ByteStrings.
pullImage :: (MonadReader DockerClientConfig m, MonadResource m)
    => [(BS.ByteString, Maybe BS.ByteString)]
    -> m (Response (ResumableSource m BS.ByteString))
pullImage queryParams = do
    config <- ask
    let host= clientHost config
    let manager = clientManager config
    initReq <- liftIO $ reqBuilder host queryParams
    http initReq{ method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                } manager
  where
    reqBuilder host qps = do
        req <- parseUrl $ (exportHost host) ++ "/images/create"
        return $ setQueryString qps req

