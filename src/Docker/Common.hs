
module Docker.Common where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Network.Wreq

-- | GET with content-type JSON
getAsJson :: FromJSON a => String -> String -> IO (Response a)
getAsJson baseUrl endPoint = liftIO $
    get (baseUrl ++ endPoint) >>= asJSON

