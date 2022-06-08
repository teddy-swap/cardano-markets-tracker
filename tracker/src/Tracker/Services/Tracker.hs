module Tracker.Services.Tracker where

import CardanoTx.Models

import Explorer.Service
import Explorer.Models
import Explorer.Class
import Explorer.Types

import System.Logging.Hlog
import RIO
import GHC.Natural

import Tracker.Caches.Cache
import Tracker.Models.AppConfig

data TrackerService f = TrackerService
  { getAllTransactions :: f ([CompletedTx], Int)
  }

mkTrackerService
  :: (Monad i, MonadIO f)
  => TrackerSettings
  -> MakeLogging i f
  -> Cache f
  -> Explorer f
  -> i (TrackerService f)
mkTrackerService settings MakeLogging{..} cache explorer = do
  logger <- forComponent "trackerService"
  pure $ TrackerService $ getAllTransactions' explorer cache logger settings  

getAllTransactions' 
  :: (MonadIO f) 
  => Explorer f
  -> Cache f
  -> Logging f
  -> TrackerSettings
  -> f ([CompletedTx], Int)
getAllTransactions' Explorer{..} Cache{..} Logging{..} TrackerSettings{..} = do
  _ <- infoM @String "Going to get next tracker iteration."
  lastIndex <- getLastIndex
  Items{..} <- getTxs (Paging lastIndex (naturalToInt limit)) Asc
  _ <- infoM $ "Got next txn batch: " ++ show items
  let 
    cardanoTxn = fmap toCardanoTx items
    newIndex = total + lastIndex + 1
  _ <- infoM $ "Gardano txn are: " ++ show cardanoTxn ++ ". New index is: " ++ show newIndex
  
  return (cardanoTxn, newIndex)

