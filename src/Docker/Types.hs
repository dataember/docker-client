{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    #-}

module Docker.Types where
import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Default (def, Default)
import Data.Map as Map
import Data.Text as T
import GHC.Generics
import Network.Wreq

import Network.URL
import qualified Network.Wreq.Session as S

data DockerClientConfig = DockerClientConfig
    { hostname :: Host
    , session  :: S.Session
    } deriving (Show)

data ContainerSpec = ContainerSpec
    { chostname        :: T.Text
    , domainName      :: T.Text
    , user            :: T.Text
    , attachedStdin   :: Bool
    , attachedStdout  :: Bool
    , attachedStderr  :: Bool
    , tty             :: Bool
    , openStdin       :: Bool
    , stdinOnce       :: Bool
    , env             :: Value -- possibly null
    , cmd             :: [T.Text]
    , entryPoint      :: T.Text
    , image           :: T.Text
    , labels          :: Map.Map T.Text T.Text
    , volumes         :: Map.Map T.Text T.Text
    , workingDir      :: T.Text -- should be a path
    , networkDisabled :: Bool
    , macAddress      :: T.Text -- likely already MAC addr type
    , exposedPorts    :: Map.Map T.Text Object -- according to docs, Object here is always empty...
    , securityOpts    :: [T.Text]
    , hostConfig      :: HostConfig
    } deriving (Eq, Show)

instance Default ContainerSpec where
    def = ContainerSpec
        { chostname     = ""
        , domainName    = ""
        , user          = ""
        , attachedStdin = False
        , attachedStdout= True
        , attachedStderr= True
        , tty           = False
        , openStdin     = False
        , stdinOnce     = False
        , env           = Null
        , cmd           = ["echo", "test"]
        , entryPoint    = ""
        , image         = "scratch"
        , labels        = Map.empty
        , volumes       = Map.empty
        , workingDir    = ""
        , networkDisabled = False
        , macAddress    = "00:00:00:00:00:00"-- maybe this is auto if empty???
        , exposedPorts  = Map.empty
        , securityOpts  = [""]
        , hostConfig    = def
        }

-- This would be generic, except for some reason Docker devs
-- decided it would be a good idea to have case sensitive JSON
-- nbjects for their API's!!!! Maybe there is a good reason,
-- I haven't seen one yet.
instance FromJSON ContainerSpec where
    parseJSON (Object x) = ContainerSpec
        <$> x .: "HostName"
        <*> x .: "DomainName"
        <*> x .: "User"
        <*> x .: "AttachedStdin"
        <*> x .: "AttachedStdout"
        <*> x .: "AttachedStderr"
        <*> x .: "Tty"
        <*> x .: "OpenStdin"
        <*> x .: "StdinOnce"
        <*> x .: "Env"
        <*> x .: "Cmd"
        <*> x .: "EntryPoint"
        <*> x .: "Image"
        <*> x .: "Labels"
        <*> x .: "Volumes"
        <*> x .: "WorkingDir"
        <*> x .: "NetworkDisabled"
        <*> x .: "MacAddress"
        <*> x .: "ExposedPorts"
        <*> x .: "SecurityOpts"
        <*> x .: "HostConfig"
    parseJSON _ = fail "Expecting an Object!"

instance ToJSON ContainerSpec where
    toJSON (ContainerSpec a b c d e f g h i j k l m n o p q r s t u) =
        object [ "HostName"       .= a
               , "DomainName"     .= b
               , "User"           .= c
               , "AttachedStdin"  .= d
               , "AttachedStdout" .= e
               , "AttachedStderr" .= f
               , "Tty"            .= g
               , "OpenStdin"      .= h
               , "StdinOnce"      .= i
               , "Env"            .= j
               , "Cmd"            .= k
               , "EntryPoint"     .= l
               , "Image"          .= m
               , "Labels"         .= n
               , "Volumes"        .= o
               , "WorkingDir"     .= p
               , "NetworkDisabled".= q
               , "MacAddress"     .= r
               , "ExposedPorts"   .= s
               , "SecurityOpts"   .= t
               , "HostConfig"     .= u
               ]


data HostConfig = HostConfig
    { binds           :: [T.Text] -- see binds, and create a type
    , links           :: [T.Text]
    , lxcConf         :: Map.Map T.Text T.Text
    , memory          :: Int
    , memorySwap      :: Int
    , cpuShares       :: Int
    , cpusetCpus      :: T.Text -- create a type
    , portBindings    :: Map.Map T.Text [Map.Map T.Text T.Text] -- Why is it defined like this!!!
    , publishAllPorts :: Bool
    , privileged      :: Bool
    , readOnlyRootfs  :: Bool
    , dns             :: [T.Text]
    , dnsSearch       :: [T.Text] -- Search domains
    , extraHosts      :: Value -- ["hostname:ip"], can be null???
    , volumesFrom     :: [T.Text]
    , capAdd          :: [T.Text]
    , capDrop         :: [T.Text]
    , restartPolicy   :: Map.Map T.Text T.Text -- only 2 keys possible
    , networkMode     :: T.Text
    , devices         :: [HCDevice]
    , ulimits         :: [Map.Map T.Text T.Text]
    , logConfig       :: HCLogConfig
    , cgroupParent    :: T.Text -- path
    } deriving (Eq, Generic, Show)

instance Default HostConfig where
    def = HostConfig
        { binds        = []
        , links        = []
        , lxcConf      = Map.empty
        , memory       = 0
        , memorySwap   = 0
        , cpuShares    = 512
        , cpusetCpus   = "0,1"
        , portBindings = Map.empty
        , publishAllPorts = False
        , privileged      = False
        , readOnlyRootfs  = False
        , dns             = ["8.8.8.8"]
        , dnsSearch       = []
        , extraHosts      = Null
        , volumesFrom     = []
        , capAdd          = []
        , capDrop         = []
        , restartPolicy   = Map.fromList [("Name", ""), ("MaximumRetryCount","0")]
        , networkMode     = "bridge"
        , devices         = []
        , ulimits         = []
        , logConfig       = def
        , cgroupParent    = ""
        }

instance FromJSON HostConfig

instance ToJSON HostConfig where
    toJSON (HostConfig a b c d e f g h i j k l m n o p q r s t u v w) =
        object [ "Binds" .= a
               , "Links" .= b
               , "LxcConf" .= c
               , "Memory" .= d
               , "MemorySwap" .= e
               , "CpuShares" .= f
               , "CpusetCpus" .= g
               , "PortBindings" .= h
               , "PublishAllPorts" .= i
               , "Privileged" .= j
               , "ReadOnltRootfs" .= k
               , "Dns" .= l
               , "DnsSearch" .= m
               , "ExtraHosts" .= n
               , "VolumesFrom" .= o
               , "CappAdd" .= p
               , "CapDrop" .= q
               , "RestartPolicy" .= r
               , "NetworkMode" .= s
               , "Devices" .= t
               , "Ulimits" .= u
               , "LogConfig" .= v
               , "CgroupParent" .= w
               ]


data HCDevice = HCDevice
    { pathOnHost        :: T.Text
    , pathInContainer   :: T.Text
    , cgroupPermissions :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON HCDevice
instance ToJSON HCDevice where
    toJSON (HCDevice poh pic cgp) =
        object [ "PathOnHost"        .= poh
               , "PathInContainer"   .= pic
               , "CgroupPermissions" .= cgp
               ]



data HCLogConfig = HCLogConfig
    { driverType   :: T.Text
    , config :: Map.Map T.Text T.Text
    } deriving (Eq, Generic, Show)


instance Default HCLogConfig where
    def = HCLogConfig
        { driverType = "json-file"
        , config     = Map.empty
        }

instance FromJSON HCLogConfig
instance ToJSON HCLogConfig where
    toJSON (HCLogConfig driver config) =
        object [ "Type"   .= driver
               , "Config" .= config
               ]


