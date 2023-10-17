module Tracker.Models.OnChainEvent
  ( OnChainEvent(..)
  ) where

import Cardano.Api (SlotNo)
import ErgoDex.State
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data OnChainEvent a = OnChainEvent (OnChain a) SlotNo
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
