module Tracker.Models.Events.PoolEvent where

import Streaming.Class

import Tracker.Models.Interop.Pool

import Data.Aeson
import GHC.Generics
import Prelude 
import RIO

data PoolEvent = PoolEvent
  { pool      :: Pool
  , timestamp :: Integer
  } deriving (Generic, FromJSON, ToJSON)
