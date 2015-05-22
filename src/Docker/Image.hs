
module Docker.Image where

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


-- | List all images cached on the host
listImages :: DockerClient [Map.Map T.Text Value]
listImages = do
    cfg <- ask
    r   <- lift $ getAsJson (apiBase cfg) "/images/json"
    return $ r ^. responseBody

