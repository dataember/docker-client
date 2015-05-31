{-# LANGUAGE
    OverloadedStrings
    #-}

module Docker.Conduit.Client where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Data.Monoid
import Network.HTTP.Conduit hiding (Proxy)
import Network.URL

import Docker.Conduit.Info
import Docker.Conduit.Image
import Docker.Conduit.Types

runClient :: MonadResource m => DockerClientConfig -> DockerClient m a -> m a
runClient cfg = flip runReaderT cfg

testClient :: IO ()
testClient = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    let dockerHost = Host (HTTP False) ("192.168.1.2") (Just 2375)
    let cfg = DockerClientConfig manager dockerHost
    let queryParams = [("fromSrc", Just "-"), ("tag", Just "1.0.0")]
    tar <- liftIO $ BL.readFile "/var/tmp/postgres.tar"
    runClient cfg $ do
        importContainerImageFromTar queryParams tar >>= \s -> responseBody s $$+- CC.stdout

