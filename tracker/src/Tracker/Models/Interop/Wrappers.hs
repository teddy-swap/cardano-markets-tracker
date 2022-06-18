module Tracker.Models.Interop.Wrappers where

import Plutus.V1.Ledger.Value as Value
import PlutusTx.Builtins.Internal

import Tracker.Models.Interop.Class

import RIO
import Data.Aeson

import ErgoDex.Contracts.Types
import ErgoDex.Amm.Pool 
import ErgoDex.Types

newtype TokenNameWrapper = TokenNameWrapper
  { unTokenName :: BuiltinByteString
  }
  deriving (Eq, Generic) 
  deriving newtype (Show, FromJSON, ToJSON)

newtype CurrencySymbolWrapper = CurrencySymbolWrapper
  { unCurrencySymbol :: BuiltinByteString
  } 
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)

data AssetClassWrapper = AssetClassWrapper 
  { tokenName      :: TokenNameWrapper
  , currencySymbol :: CurrencySymbolWrapper
  } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance CardanoWrapper AssetClass AssetClassWrapper where
  wrap (AssetClass pair) =
      AssetClassWrapper wrappedTn wrappedCs
    where
      wrappedCs = CurrencySymbolWrapper $ Value.unCurrencySymbol $ RIO.fst pair
      wrappedTn = TokenNameWrapper $ Value.unTokenName $ RIO.snd pair

instance CardanoWrapper PoolId AssetClassWrapper where
  wrap (PoolId ac) =
      wrap ac

instance CardanoWrapper (Coin a) AssetClassWrapper where
  wrap (Coin ac) =
      wrap ac

data AssetAmountWrapper = AssetAmountWrapper
  { asset  :: AssetClassWrapper
  , amount :: Integer
  } deriving (Generic, Show, Eq, ToJSON, FromJSON)

instance CardanoWrapper (AssetAmount a) AssetAmountWrapper where
  wrap (AssetAmount asset num) =
      AssetAmountWrapper 
        { asset  = wrap asset
        , amount = unAmount num
        }

instance CardanoWrapper AssetEntry AssetAmountWrapper where
  wrap (AssetEntry pair) =
      AssetAmountWrapper 
        { asset  = wrap $ RIO.fst pair
        , amount = RIO.snd pair
        }

data PoolFeeWrapper = PoolFeeWrapper
  { poolFeeNum :: Integer
  , poolFeeDen :: Integer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance CardanoWrapper PoolFee PoolFeeWrapper where
  wrap (PoolFee poolFeeNum poolFeeDen) =
      PoolFeeWrapper 
        { poolFeeNum = poolFeeNum
        , poolFeeDen = poolFeeDen
        }
