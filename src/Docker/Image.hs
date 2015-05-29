{-# LANGUAGE
    DataKinds
    , EmptyDataDecls
    , GADTs
    , OverloadedStrings
    , TypeFamilies
    #-}

module Docker.Image where

import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy
import Data.Map as Map
import qualified Data.Text as T
import Network.URL
import Network.Wreq
import Network.Wreq.Types


-- | Create an image.
createImage :: Postable a => Host -> Options -> Maybe a -> IO (Response ByteString)
createImage host opts maybePayload =
    case maybePayload of
        Just p -> post' host opts p
        Nothing-> post' host opts emptyObject
  where
    post' host opts = postWith opts (show host  ++ "/images/create")


-- | Retrieve info all images cached on the host
listImages :: Host -> Options -> IO (Response ByteString)
listImages host opts =
     getWith opts (show host ++ "/images/json")


-- Inspect an image.
inspectImage :: Host -> Options -> String -> IO (Response ByteString)
inspectImage host opts name =
    getWith opts (show host ++ "/images/" ++ name ++ "/json")


-- | Get the history of an image.
imageHistory :: Host -> String -> IO (Response ByteString)
imageHistory host name =
    get (show host ++ "/images/" ++ name ++ "/history")


-- | Push an image to the registry it is tagged with.
pushImage :: Host -> Options -> Maybe String -> String -> IO (Response ByteString)
pushImage host opts maybeRepo name =
    case maybeRepo of
        Just repo ->
            postWith opts (genEndpoint host $ repo ++ "/" ++ name) emptyObject
        Nothing   -> postWith opts (genEndpoint host $ name) emptyObject
  where
    genEndpoint :: Host -> String -> String
    genEndpoint host s = show host ++ "/images/" ++ s ++ "/push"


-- | Tag an images with the given repository name.
tagImage :: Host -> Options -> String -> IO (Response ByteString)
tagImage host opts name =
    postWith opts (show host ++ "/images/" ++ name ++ "/tag") emptyObject


-- | Delete an image.
deleteImage :: Host -> Options -> String -> IO (Response ByteString)
deleteImage host opts name =
    deleteWith opts (show host ++ "/images/" ++ name)


