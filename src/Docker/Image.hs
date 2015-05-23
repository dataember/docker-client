{-# LANGUAGE OverloadedStrings #-}

module Docker.Image where

import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens.Operators
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.ByteString.Lazy as BL
import Data.Map as Map
import qualified Data.Text as T
import Network.Wreq

import Docker.Common
import Docker.Types


-- | Pull an image from docker hub
-- FIXME : Need to handle this properly, this call blocks
-- and multiple "chunks" of data, in the form status updates,
-- are sent from the server.
createImageBlocking :: T.Text -> DockerClient Value
createImageBlocking from = do
    cfg <- ask
    lift $ do
        rsp <- asJSON =<< post (apiBase cfg ++ "/images/create") ["fromImage" := from]
        return $ rsp ^. responseBody



-- | Retrieve info all images cached on the host
listImages :: DockerClient (Either Int [Map.Map T.Text Value])
listImages = do
    cfg <- ask
    lift $ (getJson defaults $ apiBase cfg ++ "/images/json") >>=
        \r -> return $ getResponse r
  where
    getResponse :: Response a -> Either Int a
    getResponse r =
        let status = r ^. responseStatus . statusCode in
            case (status) of
                200 -> Right $ r ^. responseBody
                _   -> Left status



