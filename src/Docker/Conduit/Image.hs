{-# LANGUAGE
    FlexibleContexts
    , OverloadedStrings
    #-}

module Docker.Conduit.Image where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)
import Network.URL

import Docker.Conduit.Types

import Debug.Trace


-- | Pull an image of the given name and tag from the specified
-- repo and registry.
--
-- FIXME : The parameters here should be typed, not just ByteStrings.
pullImage :: (MonadReader DockerClientConfig m, MonadResource m)
    => [(BS.ByteString, Maybe BS.ByteString)] -- ^ Query params
    -> m (Response (ResumableSource m BS.ByteString))
pullImage queryParams = do
    config <- ask
    let host= clientHost config
    let manager = clientManager config
    initReq <- liftIO $ reqBuilder host queryParams
    http initReq manager
  where
    reqBuilder host qps = do
        req <- parseUrl $ (exportHost host) ++ "/images/create"
        return $ setQueryString qps req
                { method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                }


-- | Load a tarball with a set of images and tags into docker.
--
-- This call does not return any information on the image after
-- loading.
loadImagesFromTar :: (MonadReader DockerClientConfig m, MonadResource m)
    => BL.ByteString
    -> m (Response (ResumableSource m BS.ByteString))
loadImagesFromTar tar = do
    config <- ask
    let host = clientHost config
    let manager = clientManager config
    initReq <- liftIO $ reqBuilder host
    http (traceShow initReq $ initReq)  manager
  where
    reqBuilder host = do
        req <- parseUrl $ (exportHost host) ++ "/images/load"
        return $ req { requestBody = requestBodySourceChunked $ CB.sourceLbs tar
                     , method = "POST"
                     , redirectCount = 0
                     , checkStatus = \_ _ _ -> Nothing
                     }


-- |
-- Endpoint : /images/create
-- Method   : POST
--
-- Imports an image which was previously exported with
-- /containers/(containerId)/export.
--
-- There are two endpoints for importing "images". This endpoint, which
-- imports a tar created from a container export (see /containers/(id)/export)
-- and the /images/load endpoint which imports a tar of an actual image
-- (see /images/(name)/get).
--
-- @
--  ...
--  tar <- liftIO $ readFile "/var/tmp/containerExport.tar"
--  ...
--  importContainerAsImageFromTar
--      [("tag", Just "1.0.0")]
--      tar
--  ...
-- @
--
importContainerAsImageFromTar :: (MonadReader DockerClientConfig m, MonadResource m)
    => [(BS.ByteString, Maybe BS.ByteString)] -- ^ Needs to be typed as fromSrc is always "-"!
    -> BL.ByteString
    -> m (Response (ResumableSource m BS.ByteString))
importContainerAsImageFromTar queryParams tar =do
    config <- ask
    let host = clientHost config
    let manager = clientManager config
    initReq <- liftIO $ reqBuilder host queryParams tar
    traceShow initReq $ http initReq manager
  where
    reqBuilder host qps tar = do
        req <- parseUrl $ (exportHost host) ++ "/images/create"
        return $ setQueryString (("fromSrc", Just "-"):qps) $
            req { requestBody =  requestBodySourceChunked $ CB.sourceLbs tar
                , method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                }

