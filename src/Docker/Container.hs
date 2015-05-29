{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    #-}

module Docker.Container where

import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text as T
import Network.Wreq
import qualified Network.Wreq.Session as S
import Docker.Types


createContainer :: DockerClientConfig -> Options -> ContainerSpec -> IO (Response ByteString)
createContainer conf opts cspec =
    let host = hostname conf
        sess = session conf
    in  S.postWith opts sess (show host ++ "/containers/create") (toJSON cspec)


-- | Retrieve info on all running containers.
listRunningContainers :: DockerClientConfig -> Options -> IO (Response ByteString)
listRunningContainers conf opts =
    let host = hostname conf
        sess = session conf
    in  S.getWith opts sess (show host ++ "/containers/json")

