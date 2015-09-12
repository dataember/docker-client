{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , ScopedTypeVariables
    , TemplateHaskell
    , TypeFamilies
    #-}

module Docker.Language where

import Data.Aeson
import Data.Proxy
import Data.Singletons.TH
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Docker.JSON.Types.Container

-- | FIXME : Move these!
data DaemonAddress = DaemonAddress
    { daemonHost :: BC.ByteString
    , daemonPort :: Int
    } deriving (Eq, Show)


-- * Endpoints
$(singletons [d|
    data ApiEndpoint =
        InfoEndpoint
        | ContainerCreateEndpoint
        | ContainerInfoEndpoint
        | ContainerStartEndpoint
        | ContainerStopEndpoint
        | ContainerExecInitEndpoint
        | ExecStartEndpoint
        | ImageCreateEndpoint
        | ImageListEndpoint
        | ImageInfoEndpoint
    |])

-- ** Request types for endpoints

-- | The ID of a container.
type ContainerId = BC.ByteString

-- | The ID of an image.
type ImageId = BC.ByteString

-- | The ID of an exec.
type ExecId = BC.ByteString

data ContainerExecInitRequest = ContainerExecInitRequest
    { containerId :: ContainerId
    , postData    :: ContainerExecInit
    } deriving (Eq, Show)

data ExecStartEndpointRequest = ExecStartEndpointRequest
    { execId       :: ExecId
    , execPostData :: ExecStart
    } deriving (Eq, Show)



-- * Response types
type family ApiEndpointResponse (e :: ApiEndpoint) :: * where
    ApiEndpointResponse 'InfoEndpoint            = Value

    -- Container
    ApiEndpointResponse 'ContainerCreateEndpoint   = Value
    ApiEndpointResponse 'ContainerInfoEndpoint     = Value
    ApiEndpointResponse 'ContainerStartEndpoint    = ()
    ApiEndpointResponse 'ContainerStopEndpoint    = ()
    ApiEndpointResponse 'ContainerExecInitEndpoint = Value

    -- Exec
    ApiEndpointResponse 'ExecStartEndpoint = BL.ByteString

    --  Image
    ApiEndpointResponse 'ImageCreateEndpoint     = BL.ByteString
    ApiEndpointResponse 'ImageListEndpoint       = Value
    ApiEndpointResponse 'ImageInfoEndpoint       = Value



-- ** Request types

-- *** GET
type family GetEndpoint (e :: ApiEndpoint) :: * where
    GetEndpoint 'InfoEndpoint          = Proxy ()

    -- Container
    GetEndpoint 'ContainerInfoEndpoint = ContainerId

    -- Image
    GetEndpoint 'ImageListEndpoint     = Proxy () -- Should take query string
    GetEndpoint 'ImageInfoEndpoint     = ImageId




-- *** POST
type family PostEndpoint (e :: ApiEndpoint) :: * where
    -- Container
    PostEndpoint 'ContainerCreateEndpoint   = ContainerSpec
    PostEndpoint 'ContainerExecInitEndpoint = ContainerExecInitRequest
    PostEndpoint 'ContainerStartEndpoint    = ContainerId
    PostEndpoint 'ContainerStopEndpoint     = ContainerId

    -- Exec
    PostEndpoint 'ExecStartEndpoint         = ExecStartEndpointRequest

    -- Image
    PostEndpoint 'ImageCreateEndpoint       = String


data ApiF a where
    GetF
        :: SApiEndpoint e
        -> GetEndpoint e
        -> (Either String (ApiEndpointResponse e) -> a)
        -> ApiF a

    PostF
        :: SApiEndpoint e
        -> PostEndpoint e
        -> (Either String (ApiEndpointResponse e) -> a)
        -> ApiF a

instance Functor ApiF where
    fmap f (GetF e d c)  = GetF e d (f . c)
    fmap f (PostF e d c) = PostF e d (f . c)


-- ** Utilities


