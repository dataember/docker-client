
module Docker.Container where

import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens.Operators
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Map as Map
import Data.Text as T
import Network.Wreq

import Docker.Common
import Docker.Types


-- | Retrieve info on all running containers.
listRunningContainers :: DockerClient [Map.Map T.Text Value]
listRunningContainers = do
    cfg <- ask
    lift $ (getJson defaults $ apiBase cfg ++ "/containers/json") >>=
        \r -> return $ r ^. responseBody

