module Tracker.Models.Interop.Orders where

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

data Swap = Swap
  { poolId      :: AssetClassWrapper
  , baseIn      :: Amount Base
  , minQuoteOut :: Amount Quote
  , base        :: AssetClassWrapper
  , quote       :: AssetClassWrapper
  , exFee       :: ExFeePerToken
  , rewardPkh   :: PubKeyHash
  , rewardSPkh  :: Maybe StakePubKeyHash
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Deposit = Deposit
  { poolId        :: AssetClassWrapper
  , x             :: AssetAmountWrapper
  , y             :: AssetAmountWrapper
  , exFee         :: ExFee
  , rewardPkh     :: PubKeyHash
  , rewardSPkh    :: Maybe StakePubKeyHash
  , adaCollateral :: Amount Lovelace
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Redeem = Redeem
  { poolId     :: AssetClassWrapper
  , lqIn       :: Amount Liquidity
  , lq         :: AssetClassWrapper
  , exFee      :: ExFee
  , rewardPkh  :: PubKeyHash
  , rewardSPkh :: Maybe StakePubKeyHash
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)