{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , KindSignatures
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Response where
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types.Status (Status(Status))

import Docker.Language

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

decodeResponse SImageCreateEndpoint resp = checkResponse resp >>=
    \_ -> Right $ responseBody resp
