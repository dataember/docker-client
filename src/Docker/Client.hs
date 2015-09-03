{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Client where

import Data.Aeson
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Conduit.Binary
import Data.Proxy
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit hiding (Proxy)

import Docker.Language
import Docker.Request
import Docker.Response
import Docker.JSON.Types
import Docker.JSON.Types.Container

getF :: SApiEndpoint e
     -> GetEndpoint e
     -> Free ApiF (Either String (ApiEndpointBase e))
getF e d = Free $ GetF e d Pure


postF ::
    SApiEndpoint e
    -> PostEndpoint e
    -> Free ApiF (Either String (ApiEndpointBase e))
postF e d = Free $ PostF e d Pure

-------------------------------------------------------------------------------
-- | Interpreter
httpDocker :: MonadIO m
    => Free ApiF a
    -> ReaderT (Manager, DaemonAddress) m a

httpDocker (Pure a) = return a

-- Get
httpDocker (Free (GetF e d c)) = do
    (manager, daddr) <- ask
    response <- liftIO $ httpLbs (getRequest e d daddr) manager
    httpDocker . c . decodeResponse e $ response

-- Post
httpDocker (Free (PostF e d c)) = do
    (manager, daddr) <- ask
    response <- liftIO $ httpLbs (postRequest e d daddr) manager
    httpDocker . c . decodeResponse e $ response

-------------------------------------------------------------------------------
-- FIXME : These should not be partial

-- | Get \/info
getInfo
    :: MonadIO m
    => ReaderT (Manager, DaemonAddress) m Object --DockerDaemonInfo
getInfo = httpDocker $ do
    info <- getF SInfoEndpoint Proxy
    return $ case info of
        Right i -> i
        Left e  -> error (show e)


-- | Post \/containers\/create
createContainer
    :: MonadIO m
    => ContainerSpec
    -> ReaderT (Manager, DaemonAddress) m Object -- ContainerCreateResponse
createContainer cspec = httpDocker $ do
    resp <-  postF SContainerCreateEndpoint cspec
    return $ case resp of
        Right r -> r
        Left e  -> error (show e)

inspectContainer
    :: MonadIO m
    => BC.ByteString
    -> ReaderT (Manager, DaemonAddress) m Object -- ContainerInfo
inspectContainer cid = httpDocker $ do
    resp <- getF SContainerInfoEndpoint cid
    return $ case resp of
        Right r -> r
        Left e  -> error (show e)

createImage
    :: MonadIO m
    => String
    -> ReaderT (Manager, DaemonAddress) m BL.ByteString
createImage image = httpDocker $  do
    resp <- postF SImageCreateEndpoint image
    return $ case resp of
        Right r -> r
        Left e  -> error (show e)
