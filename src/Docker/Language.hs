{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Language where

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

import Docker.JSON.Types
import Docker.JSON.Types.Container

import Debug.Trace

data DaemonAddress = DaemonAddress
    { daemonHost :: BC.ByteString
    , daemonPort :: Int
    } deriving (Eq, Show)



-- * Language for the Docker Remote API
data ApiEndpoint =
    InfoEndpoint
    | ContainerEndpoint
    | ContainerInfoEndpoint

data SApiEndpoint (e :: ApiEndpoint) :: * where
    SInfoEndpoint       :: SApiEndpoint 'InfoEndpoint
    SContainerEndpoint  :: SApiEndpoint 'ContainerEndpoint
    SContainerInfoEndpoint:: SApiEndpoint 'ContainerInfoEndpoint

type family ApiEndpointBase (e :: ApiEndpoint) :: * where
    ApiEndpointBase 'InfoEndpoint       = DockerDaemonInfo
    ApiEndpointBase 'ContainerEndpoint  = ContainerCreateResponse
    ApiEndpointBase 'ContainerInfoEndpoint  = ContainerInfo

type family GetEndpoint (e :: ApiEndpoint) :: * where
    GetEndpoint 'InfoEndpoint = Proxy ()
    GetEndpoint 'ContainerInfoEndpoint = BS.ByteString

type family PostEndpoint (e :: ApiEndpoint) :: * where
    PostEndpoint 'ContainerEndpoint = ContainerSpec


-- | API
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



