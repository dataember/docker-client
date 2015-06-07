{-# LANGUAGE
    OverloadedStrings
    , RecordWildCards
    #-}

module Docker.JSON.Types where

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T


-------------------------------------------------------------------------------
-- * Other
-- ** \/info
data DockerDaemonInfo = DockerDaemonInfo
    { daemonNumContainers :: Int
    , daemonIsDebug       :: Int
    , daemonRootDir       :: T.Text
    , daemonDriver        :: T.Text
    , daemonDriverStatus  :: [(T.Text, T.Text)]
    , daemonExecDriver    :: T.Text
    , daemonId            :: T.Text
    , daemonIPv4Forward   :: Int
    , daemonNumImages     :: Int
    , daemonIndexServerAddr :: T.Text -- ^ URL?
    , daemonInitPath      :: T.Text
    , daemonInitSha1      :: T.Text
    , daemonKernelVersion :: T.Text
    , daemonLabels        :: Maybe (Map.Map T.Text T.Text)
    , daemonMemTotal      :: Integer
    , daemonMemLimit      :: Integer
    , daemonNumCpu        :: Int
    , daemonNumEventsListener :: Int
    , daemonNumFileDesc   :: Int
    , daemonNumGoRoutines :: Int
    , daemonName          :: T.Text
    , daemonOS            :: T.Text
    , daemonRegistryConfig:: Value
    -- ^ No idea what to do with this, it's not documented in the v1.18 api...
    , daemonSwapLimit     :: Int
    , daemonSystemTime    :: T.Text
    } deriving (Eq, Show)


instance FromJSON DockerDaemonInfo where
    parseJSON (Object x) = DockerDaemonInfo
        <$> x .: "Containers"
        <*> x .: "Debug"
        <*> x .: "DockerRootDir"
        <*> x .: "Driver"
        <*> x .: "DriverStatus"
        <*> x .: "ExecutionDriver"
        <*> x .: "ID"
        <*> x .: "IPv4Forwarding"
        <*> x .: "Images"
        <*> x .: "IndexServerAddress"
        <*> x .: "InitPath"
        <*> x .: "InitSha1"
        <*> x .: "KernelVersion"
        <*> x .: "Labels"
        <*> x .: "MemTotal"
        <*> x .: "MemoryLimit"
        <*> x .: "NCPU"
        <*> x .: "NEventsListener"
        <*> x .: "NFd"
        <*> x .: "NGoroutines"
        <*> x .: "Name"
        <*> x .: "OperatingSystem"
        <*> x .: "RegistryConfig"
        <*> x .: "SwapLimit"
        <*> x .: "SystemTime"

instance ToJSON DockerDaemonInfo where
    toJSON (DockerDaemonInfo {..}) =
        object [ "Containers"           .= daemonNumContainers
               , "Debug"                .= daemonIsDebug
               , "DockerRootDir"        .= daemonRootDir
               , "Driver"               .= daemonDriver
               , "DriverStatus"         .= daemonDriverStatus
               , "ExecutionDriver"      .= daemonExecDriver
               , "ID"                   .= daemonId
               , "IPv4Forwarding"       .= daemonIPv4Forward
               , "Images"               .= daemonNumImages
               , "IndexServerAddress"   .= daemonIndexServerAddr
               , "InitPath"             .= daemonInitPath
               , "InitSha1"             .= daemonInitSha1
               , "KernelVersion"        .= daemonKernelVersion
               , "Labels"               .= daemonLabels
               , "MemTotal"             .= daemonMemTotal
               , "MemLimit"             .= daemonMemLimit
               , "NCPU"                 .= daemonNumCpu
               , "EventsListener"       .= daemonNumEventsListener
               , "NFd"                  .= daemonNumFileDesc
               , "NGoroutines"          .= daemonNumGoRoutines
               , "Name"                 .= daemonName
               , "OperatingSystem"      .= daemonOS
               , "RegistryConfig"       .= daemonRegistryConfig
               , "SwapLimit"            .= daemonSwapLimit
               , "SystemTime"           .= daemonSystemTime
               ]













