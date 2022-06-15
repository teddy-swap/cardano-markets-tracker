module Tracker.Models.Interop.Pool where

import Tracker.Models.Interop.Wrappers

import Data.Aeson   (FromJSON, ToJSON)
import Ledger           
import GHC.Generics (Generic)
import RIO

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import ErgoDex.State
import ErgoDex.Amm.Pool
import ErgoDex.Class
import ErgoDex.Types

data Pool = Pool
  { id        :: AssetClassWrapper
  , reservesX :: Amount X
  , reservesY :: Amount Y
  , liquidity :: Amount Liquidity
  , x         :: AssetClassWrapper
  , y         :: AssetClassWrapper
  , lq        :: AssetClassWrapper
  , fee       :: PoolFee
  , outCollateral :: Amount Lovelace
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)