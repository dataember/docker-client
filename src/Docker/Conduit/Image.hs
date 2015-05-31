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


pullImage :: (MonadReader DockerClientConfig m, MonadResource m) =>
    BS.ByteString -> m (Response (ResumableSource m BS.ByteString))
pullImage name = do
    config <- ask
    let host= clientHost config
    let manager = clientManager config
    initReq <- liftIO $ parseUrl $ (exportHost host) ++ "/images/create"
    http initReq{ method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                , queryString =  "fromImage=" <> name
                } manager


