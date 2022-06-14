module Tracker.Models.AppConfig where

import Dhall
import RIO
import Explorer.Config
import System.Logging.Hlog
import Streaming.Config

data RedisSettings = RedisSettings
  { redisHost :: String
  , redisPort :: String 
  } deriving (Generic)

instance FromDhall RedisSettings

data TrackerSettings = TrackerSettings
  { limit :: Natural
  } deriving (Generic)

instance FromDhall TrackerSettings

data TrackerProgrammConfig = TrackerProgrammConfig
  { pollTime :: Natural
  } deriving (Generic)

instance FromDhall TrackerProgrammConfig

data RetryConfig = RetryConfig
  { sleepTime :: Natural
  } deriving (Generic)

instance FromDhall RetryConfig

data AppConfig = AppConfig
  { explorerConfig        :: ExplorerConfig
  , ordersProducerConfig  :: KafkaProducerConfig
  , ordersTopicName       :: Text
  , trackerProgrammConfig :: TrackerProgrammConfig
  , trackerSettings       :: TrackerSettings
  , redisSettings         :: RedisSettings
  , retry                 :: RetryConfig
  , loggingConfig         :: LoggingConfig
  } deriving (Generic)

instance FromDhall AppConfig