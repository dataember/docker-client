{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Client where
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import Data.Aeson
import Data.Aeson.Types (parseEither, Object, Value)
import Data.Proxy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Data.Default
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types.Status (Status(Status))


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
-- | Get \/info
-- FIXME : This should not be partial
getInfo :: Free ApiF DockerDaemonInfo
getInfo = do
    info <- getF SInfoEndpoint Proxy
    return $ case info of
        Right i -> i
        Left e  -> error (show e)


-- | Post \/containers\/create
createContainer
    :: ContainerSpec
    -> Free ApiF ContainerCreateResponse
createContainer cspec = do
    resp <-  postF SContainerCreateEndpoint cspec
    return $ case resp of
        Right r -> r
        Left e  -> error (show e)

inspectContainer
    :: BC.ByteString
    -> Free ApiF ContainerInfo
inspectContainer cid = do
    resp <- getF SContainerInfoEndpoint cid
    return $ case resp of
        Right r -> r
        Left e  -> error (show e)
