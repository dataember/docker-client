
module Docker.Common where


import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens.Operators
import Data.Aeson
import Network.Wreq

import Docker.Types


-- | GET with content-type JSON
getJson :: FromJSON a => Options -> String -> IO (Response a)
getJson opts url = asJSON =<< getWith opts url


