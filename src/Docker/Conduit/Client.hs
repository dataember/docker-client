{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Conduit.Client where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import Data.Aeson (decode,encode,json)
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

import Docker.Conduit.Info
import Docker.Conduit.Image
import Docker.Conduit.Types
import Docker.JSON.Types

runClient :: MonadResource m => DockerClientConfig -> DockerClient m a -> m a
runClient cfg = flip runReaderT cfg


data DaemonAddress = DaemonAddress
    { daemonHost :: BC.ByteString
    , daemonPort :: Int
    } deriving (Eq, Show)

-- * Temp types, should be moved


-- * Request builders
defaultRequest :: Request
defaultRequest =
    def { redirectCount = 0
        , checkStatus = \_ _ _ -> Nothing
        }

-- | Build a request for \/info
getInfoRequest :: DaemonAddress -> Request
getInfoRequest da =
    defaultRequest {
        method = "GET"
        , host = daemonHost da
        , port = daemonPort da
        , path = "/info"
        }

postContainerRequest :: DaemonAddress -> Request
postContainerRequest da =
    defaultRequest
        { method = "POST"
        , host   = daemonHost da
        , port   = daemonPort da
        , path   = "/containers/create"
        }

-- * Language for the Docker Remote API
data ApiEndpoint =
    InfoEndpoint
    | ContainerEndpoint

data SApiEndpoint (e :: ApiEndpoint) :: * where
    SInfoEndpoint       :: SApiEndpoint 'InfoEndpoint
    SContainerEndpoint  :: SApiEndpoint 'ContainerEndpoint

type family ApiEndpointBase (e :: ApiEndpoint) :: * where
    ApiEndpointBase 'InfoEndpoint       = DockerDaemonInfo
    ApiEndpointBase 'ContainerEndpoint  = Map.Map String String -- ^ needs to be a sum type

type family GetEndpoint (e :: ApiEndpoint) :: * where
    GetEndpoint 'InfoEndpoint = Proxy ()

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


getF :: SApiEndpoint e
     -> GetEndpoint e
     -> Free ApiF (Either String (ApiEndpointBase e))
getF e d = Free $ GetF e d Pure


postF ::
    SApiEndpoint e
    -> PostEndpoint e
    -> Free ApiF (Either String (ApiEndpointBase e))
postF e d = Free $ PostF e d Pure

getRequest
    :: SApiEndpoint e
    -> GetEndpoint e
    -> DaemonAddress
    -> Request
getRequest SInfoEndpoint d addr = getInfoRequest addr


postRequest
    :: SApiEndpoint e
    -> PostEndpoint e
    -> DaemonAddress
    -> Request
postRequest SContainerEndpoint d addr =
    (postContainerRequest addr)
        { requestBody = RequestBodyLBS $ encode d
        }




checkResponse  :: Response a -> Either String ()
checkResponse resp = case responseStatus resp of
    (Status 200 _) -> Right ()
    _              -> Left "Error!"

-------------------------------------------------------------------------------
-- * Decode Responses

decodeResponse ::
    SApiEndpoint e
    -> Response BL.ByteString
    -> Either String (ApiEndpointBase e)

-- | \/info reponse
decodeResponse SInfoEndpoint resp = checkResponse resp >>=
    \_ -> case decode (responseBody resp) of
        Just val-> Right val
        Nothing -> Left "Error!"

-- | \/containers\/create response
decodeResponse SContainerEndpoint resp = checkResponse resp >>=
    \_ -> case decode (responseBody resp) of
        Just val -> Right val
        Nothing  -> Left "Error!"


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
        Left _  -> error ""

createContainer :: Free ApiF (Map.Map String String)
createContainer = do
    resp <- postF SContainerEndpoint _
    return $ case resp of
        Right r -> r
        Left _  -> error "Error!"
