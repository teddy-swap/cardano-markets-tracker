module Tracker.Syntax.Retry where

import RIO
import Control.Retry
import Control.Monad.Catch
import System.Logging.Hlog
import GHC.Natural

import Tracker.Models.AppConfig

execWithRetry
  :: (MonadIO f, MonadMask f)
  => Logging f
  -> RetryConfig
  -> String
  -> f a
  -> f a
execWithRetry Logging{..} RetryConfig{..} logMsg func = do
  recoverAll (constantDelay $ naturalToInt sleepTime) (\rs -> (infoM $ logMsg ++ show rs) >> func)