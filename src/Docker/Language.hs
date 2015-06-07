{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Language where

import Data.Proxy
import qualified Data.ByteString.Char8 as BC

import Docker.JSON.Types
import Docker.JSON.Types.Container


-- | FIXME : Move these!
data DaemonAddress = DaemonAddress
    { daemonHost :: BC.ByteString
    , daemonPort :: Int
    } deriving (Eq, Show)

type ContainerId = BC.ByteString

-- * Language for the Docker Remote API
data ApiEndpoint =
    InfoEndpoint
    | ContainerCreateEndpoint
    | ContainerInfoEndpoint

data SApiEndpoint (e :: ApiEndpoint) :: * where
    SInfoEndpoint           :: SApiEndpoint 'InfoEndpoint
    SContainerCreateEndpoint      :: SApiEndpoint 'ContainerCreateEndpoint
    SContainerInfoEndpoint  :: SApiEndpoint 'ContainerInfoEndpoint

-- | Bind response data for
type family ApiEndpointBase (e :: ApiEndpoint) :: * where
    ApiEndpointBase 'InfoEndpoint            = DockerDaemonInfo
    ApiEndpointBase 'ContainerCreateEndpoint = ContainerCreateResponse
    ApiEndpointBase 'ContainerInfoEndpoint   = ContainerInfo

-- | Bind request data for a GET on specific endpoint
type family GetEndpoint (e :: ApiEndpoint) :: * where
    GetEndpoint 'InfoEndpoint          = Proxy ()
    GetEndpoint 'ContainerInfoEndpoint = ContainerId

-- | bind request data for a POST on a specific endpoint
type family PostEndpoint (e :: ApiEndpoint) :: * where
    PostEndpoint 'ContainerCreateEndpoint = ContainerSpec

data ApiF a where
    GetF
        :: SApiEndpoint e
        -> GetEndpoint e
        -> (Either String (ApiEndpointBase e) -> a)
        -> ApiF a

    PostF
        :: SApiEndpoint e
        -> PostEndpoint e
        -> (Either String (ApiEndpointBase e) -> a)
        -> ApiF a

instance Functor ApiF where
    fmap f (GetF e d c)  = GetF e d (f . c)
    fmap f (PostF e d c) = PostF e d (f . c)



