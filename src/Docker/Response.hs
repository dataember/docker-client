{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Response where
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

-- | This should be per endpoint also
checkResponse  :: Response a -> Either String ()
checkResponse resp = case responseStatus resp of
    (Status 200 _) -> Right ()
    (Status 201 _) -> Right ()
    (Status 202 _) -> Right ()
    (Status 203 _) -> Right ()
    (Status 404 m) -> Left ("No such image! " ++ show m)
    (Status 500 m) -> Left ("Error 500! " ++ show m)
    (Status _ _)   -> Left ("Error! Undefined status returned...")

-------------------------------------------------------------------------------
-- * Decode Responses

decodeResponse ::
    SApiEndpoint e
    -> Response BL.ByteString
    -> Either String (ApiEndpointBase e)

-- | \/info reponse
decodeResponse SInfoEndpoint resp = checkResponse resp >>=
    \_ -> case eitherDecode (responseBody resp) of
        Right val -> Right val
        Left e    -> Left ("Error decoding! " ++ show e)

-- | \/containers\/create response
decodeResponse SContainerCreateEndpoint resp = checkResponse resp >>=
    \_ -> case eitherDecode (responseBody resp) of
        Right val -> Right val
        Left e    -> Left ("Error decoding! " ++ show e)

decodeResponse SContainerInfoEndpoint resp = checkResponse resp >>=
    \_ -> case eitherDecode (responseBody resp) of
        Right val -> Right val
        Left e    -> Left ("Error decoding! " ++ show e)
