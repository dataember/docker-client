
module Docker.Conduit.Types where

import Control.Monad.Reader
import Network.HTTP.Conduit (Manager)
import Network.URL


data DockerClientConfig = DockerClientConfig
    { clientManager :: Manager
    , clientHost    :: Host
    }


type DockerClient m a = ReaderT DockerClientConfig m a


