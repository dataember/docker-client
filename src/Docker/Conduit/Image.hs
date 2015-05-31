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

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)


pullImage :: (Monad m, MonadResource m, MonadThrow m) => Manager -> BS.ByteString -> m (Response (ResumableSource m BS.ByteString))
pullImage manager name = do
    initReq <- parseUrl "http://192.168.1.2:2375/images/create"
    http initReq{ method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                , queryString =  "fromImage=" <> name
                } manager


