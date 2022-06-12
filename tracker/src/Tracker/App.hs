module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Streaming.Producer

import Explorer.Service

import Tracker.Programs.TrackerProgram
import Tracker.Models.AppConfig
import Tracker.Services.ConfigReader
import Tracker.Services.Tracker
import Tracker.Caches.Cache

import           System.Logging.Hlog
import           RIO
import qualified Streamly.Prelude as S
import           Control.Monad.Trans.Resource
import           Control.Monad.Catch
import           Kafka.Producer

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = return $ App wire

wire :: (MonadUnliftIO f, MonadCatch f, S.MonadAsync f, MonadMask f) => f ()
wire = runResourceT $ do
  AppConfig {..}   <- lift $ read mkConfigReader
  loggingMaker     <- makeLogging loggingConfig
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  trackerCache     <- mkCache redisSettings loggingMaker
  let
    explorer        = mkExplorer explorerConfig
  trackerService  <- mkTrackerService trackerSettings retry loggingMaker trackerCache explorer
  trackerProgramm <- mkTrackerProgram trackerProgrammConfig loggingMaker trackerCache trackerService ordersProducer
  lift $ run trackerProgramm