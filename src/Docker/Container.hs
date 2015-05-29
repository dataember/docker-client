{-# LANGUAGE
    OverloadedStrings
    #-}

module Docker.Container where

import Data.Aeson
import Data.ByteString.Lazy
import qualified Data.Text as T
import Network.Wreq
import qualified Network.Wreq.Session as S
import Docker.Types

-- | Create a container.
createContainer :: DockerClientConfig -> Options -> ContainerSpec -> IO (Response ByteString)
createContainer conf opts cspec =
    applyCfg conf $ \host sess ->
        S.postWith opts sess (show host ++ "/containers/create") (toJSON cspec)


-- | Retrieve info on all running containers.
listRunningContainers :: DockerClientConfig -> Options -> IO (Response ByteString)
listRunningContainers conf opts =
    applyCfg conf $ \host sess ->
        S.getWith opts sess (show host ++ "/containers/json")

