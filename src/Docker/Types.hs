{-# LANGUAGE OverloadedStrings #-}

module Docker.Types where

import Network.URL
import qualified Network.Wreq.Session as S

data DockerClientConfig = DockerClientConfig
    { hostname :: Host
    , session  :: S.Session
    } deriving (Show)

