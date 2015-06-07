{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Request where
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
import Docker.JSON.Types
import Docker.JSON.Types.Container

import Debug.Trace



-- * Request builders
defaultRequest :: Request
defaultRequest =
    def { checkStatus = \_ _ _ -> Nothing
        , redirectCount = 0
        , requestHeaders = requestHeaders def ++
            [("content-type", "application/json")]
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
    traceShow d $
    (postContainerRequest addr)
        { requestBody = RequestBodyLBS $ encode d
        }


