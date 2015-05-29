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

data DockerClientConfig = DockerClientConfig
    { hostname :: Host
    , session  :: S.Session
    } deriving (Show)



-- | Create an image.
createImage :: Postable a => DockerClientConfig -> Options -> Maybe a -> IO (Response ByteString)
createImage conf opts maybePayload =
    case maybePayload of
        Just p -> post' conf opts p
        Nothing-> post' conf opts emptyObject
  where
    post' conf opts =
        let host = hostname conf
            sess = session conf
        in  S.postWith opts sess (show host  ++ "/images/create")

-- | Retrieve info all images cached on the host
listImages :: DockerClientConfig -> Options -> IO (Response ByteString)
listImages conf opts =
    let host = hostname conf
        sess = session conf
    in  S.getWith opts sess (show host ++ "/images/json")


-- Inspect an image.
inspectImage :: DockerClientConfig -> Options -> String -> IO (Response ByteString)
inspectImage conf opts name =
    let host = hostname conf
        sess = session conf
    in  S.getWith opts sess (show host ++ "/images/" ++ name ++ "/json")


-- | Get the history of an image.
imageHistory :: DockerClientConfig -> Options -> String -> IO (Response ByteString)
imageHistory conf opts name =
    let host = hostname conf
        sess = session conf
    in S.getWith opts sess (show host ++ "/images/" ++ name ++ "/history")


-- | Push an image to the registry it is tagged with.
pushImage :: DockerClientConfig -> Options -> Maybe String -> String -> IO (Response ByteString)
pushImage conf opts maybeRepo name =
    let host = hostname conf
        sess = session conf
    in case maybeRepo of
        Just repo ->
            S.postWith opts sess (genEndpoint host $ repo ++ "/" ++ name) emptyObject
        Nothing   ->
            S.postWith opts sess (genEndpoint host $ name) emptyObject
  where
    genEndpoint :: Host -> String -> String
    genEndpoint host s = show host ++ "/images/" ++ s ++ "/push"


-- | Tag an images with the given repository name.
tagImage :: DockerClientConfig -> Options -> String -> IO (Response ByteString)
tagImage conf opts name =
    let host = hostname conf
        sess = session conf
    in  S.postWith opts sess (show host ++ "/images/" ++ name ++ "/tag") emptyObject


-- | Delete an image.
deleteImage :: DockerClientConfig -> Options -> String -> IO (Response ByteString)
deleteImage conf opts name =
    let host = hostname conf
        sess = session conf
    in S.deleteWith opts sess (show host ++ "/images/" ++ name)

