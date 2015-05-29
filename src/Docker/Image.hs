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
import qualified Network.Wreq.Session as S
import Network.Wreq.Types


-- | Create an image.
createImage :: Postable a => Host -> S.Session -> Options -> Maybe a -> IO (Response ByteString)
createImage host sess opts maybePayload =
    case maybePayload of
        Just p -> post' host sess opts p
        Nothing-> post' host sess opts emptyObject
  where
    post' host sess opts = S.postWith opts sess (show host  ++ "/images/create")


-- | Retrieve info all images cached on the host
listImages :: Host -> S.Session -> Options -> IO (Response ByteString)
listImages host sess opts =
     S.getWith opts sess (show host ++ "/images/json")


-- Inspect an image.
inspectImage :: Host -> S.Session -> Options -> String -> IO (Response ByteString)
inspectImage host sess opts name =
    S.getWith opts sess (show host ++ "/images/" ++ name ++ "/json")


-- | Get the history of an image.
imageHistory :: Host -> S.Session -> Options -> String -> IO (Response ByteString)
imageHistory host sess opts name =
    S.getWith opts sess (show host ++ "/images/" ++ name ++ "/history")


-- | Push an image to the registry it is tagged with.
pushImage :: Host -> S.Session -> Options -> Maybe String -> String -> IO (Response ByteString)
pushImage host sess opts maybeRepo name =
    case maybeRepo of
        Just repo ->
            S.postWith opts sess (genEndpoint host $ repo ++ "/" ++ name) emptyObject
        Nothing   ->
            S.postWith opts sess (genEndpoint host $ name) emptyObject
  where
    genEndpoint :: Host -> String -> String
    genEndpoint host s = show host ++ "/images/" ++ s ++ "/push"


-- | Tag an images with the given repository name.
tagImage :: Host -> S.Session -> Options -> String -> IO (Response ByteString)
tagImage host sess opts name =
    S.postWith opts sess (show host ++ "/images/" ++ name ++ "/tag") emptyObject


-- | Delete an image.
deleteImage :: Host -> S.Session -> Options -> String -> IO (Response ByteString)
deleteImage host sess opts name =
    S.deleteWith opts sess (show host ++ "/images/" ++ name)


