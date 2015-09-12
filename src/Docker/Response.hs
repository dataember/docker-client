{-# LANGUAGE
    DataKinds
    , DeriveFunctor
    , GADTs
    , FlexibleInstances
    , KindSignatures
    , MultiParamTypeClasses
    , OverloadedStrings
    , TypeFamilies
    , TypeSynonymInstances
    #-}

module Docker.Response where
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types.Status (Status(Status))

import Docker.Language

-------------------------------------------------------------------------------
-- * Decode Responses

decodeResponse ::
    SApiEndpoint e
    -> Response BL.ByteString
    -> Either String (ApiEndpointResponse e)

-- | \/info reponse
decodeResponse ep@SInfoEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)

-- | \/containers\/create response
decodeResponse ep@SContainerCreateEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)

decodeResponse ep@SContainerInfoEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)

decodeResponse ep@SContainerExecInitEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)

decodeResponse ep@SContainerStartEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> Right ()

decodeResponse ep@SContainerStopEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> Right ()


-- Exec
decodeResponse ep@SExecStartEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> Right $ responseBody resp


-- Images
decodeResponse ep@SImageCreateEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> Right $ responseBody resp


decodeResponse ep@SImageListEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)

decodeResponse ep@SImageInfoEndpoint resp =
    checkResponseStatus ep resp >>=
        \_ -> case eitherDecode (responseBody resp) of
            Right val -> Right val
            Left e    -> Left ("Error decoding! " ++ show e)




checkResponseStatus  ::
    SApiEndpoint e
    -> Response a
    -> Either String ()

checkResponseStatus SInfoEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"


-- Container
checkResponseStatus SContainerCreateEndpoint resp =
    case responseStatus resp of
        (Status 201 _) -> Right ()
        (Status 404 m) -> Left $ show m -- no such container
        (Status 406 m) -> Left $ show m -- impossible to attach, not running
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

checkResponseStatus SContainerInfoEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status 404 m) -> Left $ show m
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

checkResponseStatus SContainerExecInitEndpoint resp =
    case responseStatus resp of
        (Status 201 _) -> Right ()
        (Status 404 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

checkResponseStatus SContainerStartEndpoint resp =
    case responseStatus resp of
        (Status 204 _) -> Right ()
        (Status 304 m) -> Left $ show m
        (Status 404 m) -> Left $ show m
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

checkResponseStatus SContainerStopEndpoint resp =
    case responseStatus resp of
        (Status 204 _) -> Right ()
        (Status 304 m) -> Left $ show m
        (Status 404 m) -> Left $ show m
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

-- Exec
checkResponseStatus SExecStartEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status 404 m) -> Left $ show m
        (Status _ _ )   -> Left $ "Error! "


-- Image
checkResponseStatus SImageCreateEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status 500 m) -> Left $ show m
        (Status _ _)   -> Left "Error! Undefined status returned!"

checkResponseStatus SImageListEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status _ m)   -> Left $ "Error! " ++ show m

checkResponseStatus SImageInfoEndpoint resp =
    case responseStatus resp of
        (Status 200 _) -> Right ()
        (Status 404 m) -> Left $ show m
        (Status 400 m) -> Left $ show m
        (Status _ m)   -> Left $ "Error! " ++ show m


