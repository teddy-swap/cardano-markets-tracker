module Tracker.Services.Tracker where

import CardanoTx.Models

import Explorer.Service
import Explorer.Models
import Explorer.Class
import Explorer.Types

import System.Logging.Hlog
import RIO
import GHC.Natural
import Control.Retry
import Control.Monad.Catch

import Tracker.Syntax.Retry
import Tracker.Caches.Cache
import Tracker.Models.AppConfig

data TrackerService f = TrackerService
  { getAllTransactions :: f ([SettledTx], Int)
  }

mkTrackerService
  :: (Monad i, MonadIO f, MonadMask f)
  => TrackerSettings
  -> MakeLogging i f
  -> Cache f
  -> Explorer f
  -> i (TrackerService f)
mkTrackerService settings MakeLogging{..} cache explorer = do
  logger <- forComponent "trackerService"
  pure $ TrackerService $ getAllTransactions' explorer cache logger settings  

getAllTransactions' 
  :: (MonadIO f, MonadMask f) 
  => Explorer f
  -> Cache f
  -> Logging f
  -> TrackerSettings
  -> f ([SettledTx], Int)
getAllTransactions' Explorer{..} Cache{..} l@Logging{..} TrackerSettings{..} = do
  _ <- infoM @String "Going to get next tracker iteration."
  lastIndex <- getLastIndex
  Items{..} <- execWithRetry l "Retrying getTxn." (getTxs (Paging lastIndex (naturalToInt limit)) Asc)
  _ <- infoM $ "Got next txn batch: " ++ (show $ length items)
  let 
    cardanoTxn = fmap toCardanoTx items
    newIndex = (length items) + lastIndex + 1
  _ <- infoM $ "New index is: " ++ show newIndex
  
  return (cardanoTxn, newIndex)

