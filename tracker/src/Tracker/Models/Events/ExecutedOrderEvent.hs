module Tracker.Models.Events.ExecutedOrderEvent where

import Streaming.Class

import Tracker.Models.Interop.Pool

import Data.Aeson
import GHC.Generics
import Prelude 
import RIO

data ExecutedOrderEvent = ExecutedOrderEvent
  { stringJson :: String
  } deriving (Generic, FromJSON, ToJSON)