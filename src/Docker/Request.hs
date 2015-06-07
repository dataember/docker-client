{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Request where
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)

import Docker.Language

import Debug.Trace



-- * Request builders
defaultRequest :: Request
defaultRequest =
    def { checkStatus = \_ _ _ -> Nothing
        , redirectCount = 0
        , requestHeaders = requestHeaders def ++
            [("content-type", "application/json")]
        }

-- | Build a GET request for \/info
getInfoRequest :: DaemonAddress -> Request
getInfoRequest da =
    defaultRequest
        { method = "GET"
        , host = daemonHost da
        , port = daemonPort da
        , path = "/info"
        }

-- | Build a GET request for \/containers\/\(id\)\/json
getContainerInfoRequest
    :: BC.ByteString -- ^ Container id
    -> DaemonAddress
    -> Request
getContainerInfoRequest cid da =
    defaultRequest
        { method = "GET"
        , host = daemonHost da
        , port = daemonPort da
        , path = "/containers/" <> cid <> "/json"
        }


-- | Build a POST request for \/container\/create
postContainerRequest :: DaemonAddress -> Request
postContainerRequest da =
    defaultRequest
        { method = "POST"
        , host   = daemonHost da
        , port   = daemonPort da
        , path   = "/containers/create"
        }

{-
data ApiRequestData = ApiRequestData
    { arPath          :: BS.ByteString
    , arRequestBody   :: BL.ByteString
    , arDaemonAddress :: DaemonAddress
    }
-}
getRequest
    :: SApiEndpoint e
    -> GetEndpoint e
    -> DaemonAddress
    -> Request
getRequest SInfoEndpoint _ addr = getInfoRequest addr
getRequest SContainerInfoEndpoint d addr = getContainerInfoRequest d addr


postRequest
    :: SApiEndpoint e
    -> PostEndpoint e
    -> DaemonAddress
    -> Request
postRequest SContainerCreateEndpoint d addr =
    traceShow d $
    (postContainerRequest addr)
        { requestBody = RequestBodyLBS $ encode d
        }


