module Tracker.Models.AppConfig where

import Dhall
import RIO

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