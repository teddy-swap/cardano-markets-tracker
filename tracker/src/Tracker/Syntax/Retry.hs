module Tracker.Syntax.Retry where

import RIO
import Control.Retry
import Control.Monad.Catch
import System.Logging.Hlog

execWithRetry
  :: (MonadIO f, MonadMask f)
  => Logging f
  -> String
  -> f (a)
  -> f (a)
execWithRetry Logging{..} logMsg func = do
  recoverAll (constantDelay 1000000) (\_ -> (infoM @String logMsg) >> func)