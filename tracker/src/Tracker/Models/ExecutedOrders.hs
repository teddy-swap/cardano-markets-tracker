module Tracker.Models.ExecutedOrders where

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import ErgoDex.State
import ErgoDex.Amm.Pool
import ErgoDex.Class
import ErgoDex.Types

import Explorer.Class

import CardanoTx.Models

import           Data.Aeson (FromJSON, ToJSON)
import qualified PlutusTx.Prelude as P    
import           Ledger           
import           GHC.Generics                (Generic)
import           RIO

data ExecutedOrder a = ExecutedOrder
  { config       :: a
  , orderInputId :: TxOutRef
  , userOutputId :: TxOutRef
  , poolOutputId :: TxOutRef
  , poolInputId  :: TxOutRef
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ExecutedSwap = ExecutedSwap
  { order       :: ExecutedOrder Swap
  , actualQuote :: Amount Quote
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedSwap where
  parseFromExplorer SettledTx{..} = do
    (OnChain swapOut swap@Swap{..}) <- findMatchedInput inputs :: Maybe (OnChain Swap)
    (OnChain prevPool _)            <- findMatchedInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)            <- findMatchedOutput outputs :: Maybe (OnChain Pool)
    userOutput                      <- findUserOutput swapRewardPkh swapRewardSPkh outputs
    let
      quote = amountOf (fullTxOutValue userOutput) swapQuote
      order = ExecutedOrder
        { config = swap
        , orderInputId = fullTxOutRef swapOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool
        }
    return 
      ExecutedSwap
        { order       = order
        , actualQuote = quote
        }

data ExecutedDeposit = ExecutedDeposit
  { order    :: ExecutedOrder Deposit
  , rewardLq :: AssetAmount Liquidity
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedDeposit where
  parseFromExplorer SettledTx{..} = do
    (OnChain depositOut deposit@Deposit{..}) <- findMatchedInput inputs :: Maybe (OnChain Deposit)
    (OnChain prevPool Pool{..})              <- findMatchedInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)                     <- findMatchedOutput outputs :: Maybe (OnChain Pool)
    userOutput                               <- findUserOutput depositRewardPkh depositRewardSPkh outputs
    let
      lqReward = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinLq
      order = ExecutedOrder
        { config = deposit
        , orderInputId = fullTxOutRef depositOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool
        }
    return 
      ExecutedDeposit
        { order    = order
        , rewardLq = lqReward
        }

data ExecutedRedeem = ExecutedRedeem
  { order   :: ExecutedOrder Redeem
  , rewardX :: AssetAmount X
  , rewardY :: AssetAmount Y
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedRedeem where
  parseFromExplorer SettledTx{..} = do
    (OnChain redeemOut redeem@Redeem{..}) <- findMatchedInput inputs :: Maybe (OnChain Redeem)
    (OnChain prevPool Pool{..})           <- findMatchedInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)                  <- findMatchedOutput outputs :: Maybe (OnChain Pool)
    userOutput                            <- findUserOutput redeemRewardPkh redeemRewardSPkh outputs
    let
      assetAmountX = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinX
      assetAmountY = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinY
      order = ExecutedOrder
        { config = redeem
        , orderInputId = fullTxOutRef redeemOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool
        }
    return 
      ExecutedRedeem
        { order   = order
        , rewardX = assetAmountX
        , rewardY = assetAmountY
        }

findMatchedInput :: forall a . (FromLedger a) => [FullTxIn] -> Maybe (OnChain a)
findMatchedInput (FullTxIn{..}:xs) =
  case parseFromLedger fullTxInTxOut of
      Just r -> Just r
      _      -> findMatchedInput xs
findMatchedInput [] = Nothing

findMatchedOutput :: forall a . (FromLedger a) => [FullTxOut] -> Maybe (OnChain a)
findMatchedOutput (x:xs) =
  case parseFromLedger x of
      Just r -> Just r
      _      -> findMatchedOutput xs
findMatchedOutput [] = Nothing

findUserOutput :: PubKeyHash -> Maybe StakePubKeyHash -> [FullTxOut] -> Maybe FullTxOut
findUserOutput key pkh (x@FullTxOut{..}:xs) =
    if (fullTxOutAddress P.== pubKeyHashAddress (PaymentPubKeyHash key) pkh) 
      then Just x 
      else (findUserOutput key pkh xs)
findUserOutput _ _ [] = Nothing
