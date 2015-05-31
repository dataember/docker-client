{-# LANGUAGE
    AllowAmbiguousTypes
    , DataKinds
    , FlexibleContexts
    , FlexibleInstances
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}

module Docker.Conduit.Container where

import Control.Applicative hiding (empty)
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.ByteString.Lazy hiding (foldl, foldl1)
import Data.ByteString.Builder
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.Combinators as CC
import Data.Proxy
import Data.Monoid
import qualified Data.Text as T
import GHC.TypeLits
import Network.HTTP.Conduit hiding (Proxy)


pullImage :: (Monad m, MonadResource m, MonadThrow m) => Manager -> BS.ByteString -> m (Response (ResumableSource m BS.ByteString))
pullImage manager name = do
    initReq <- parseUrl "http://192.168.1.2:2375/images/create"
    http initReq{ method = "POST"
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                , queryString =  "fromImage=" <> name
                } manager


