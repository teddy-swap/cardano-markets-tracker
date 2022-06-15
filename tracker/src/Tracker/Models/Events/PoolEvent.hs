module Tracker.Models.Events.PoolEvent where

import Streaming.Class

import Tracker.Models.Interop.Pool

import Data.Aeson
import GHC.Generics
import Prelude 
import RIO

import Ledger     

data PoolEvent = PoolEvent
  { pool      :: Pool
  , timestamp :: Integer
  , outputId  :: TxOutRef
  } deriving (Generic, FromJSON, ToJSON)
