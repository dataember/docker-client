{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Client where

import Control.Monad.Free
import Control.Monad.IO.Class
import Network.HTTP.Conduit hiding (Proxy)

import Docker.Language
import Docker.Request
import Docker.Response


getF :: SApiEndpoint e
     -> GetEndpoint e
     -> Free ApiF (Either String (ApiEndpointResponse e))
getF e d = Free $ GetF e d Pure


postF ::
    SApiEndpoint e
    -> PostEndpoint e
    -> Free ApiF (Either String (ApiEndpointResponse e))
postF e d = Free $ PostF e d Pure

-------------------------------------------------------------------------------
-- * Interpreter
--
-- The http interpreter for this client.
--

-- ** Examples
--
-- | Get the number of CPU's for the daemon at the address
-- __daddr__.
--
-- @
--  httpDocker (mgr , daddr) $
--      getF SInfoEndpoint Proxy >>= \rsp -> return $
--          rsp ^? _Right . key "NCPU"
-- @
--
--
httpDocker :: MonadIO m
    => (Manager, DaemonAddress)
    -> Free ApiF a
    -> m a

httpDocker _ (Pure a) = return a

-- Get
httpDocker cfg@(manager, daddr) (Free (GetF e d c)) = do
    response <- liftIO $ httpLbs (getRequest e d daddr) manager
    (httpDocker cfg) . c . decodeResponse e $ response

-- Post
httpDocker cfg@(manager, daddr) (Free (PostF e d c)) = do
    response <- liftIO $ httpLbs (postRequest e d daddr) manager
    (httpDocker cfg) . c . decodeResponse e $ response

