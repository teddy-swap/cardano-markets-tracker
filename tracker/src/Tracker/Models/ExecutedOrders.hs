module Tracker.Models.ExecutedOrders where

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import ErgoDex.State
import ErgoDex.Amm.Pool
import ErgoDex.Class
import ErgoDex.Types

import Explorer.Class

import CardanoTx.Models

import           Tracker.Models.SettledTx
import qualified Tracker.Models.Interop.Orders as Interop
import           Tracker.Models.Interop.Class
import           Tracker.Models.Interop.Wrappers 

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
  { swap        :: ExecutedOrder Interop.Swap
  , actualQuote :: Amount Quote
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedSwap where
  parseFromExplorer SettledTx{..} = do
    (OnChain swapOut swap@Swap{..}) <- findMatchedInput @Swap inputs
    (OnChain prevPool _)            <- findMatchedInput @Pool inputs
    (OnChain currPool _)            <- findMatchedOutput @Pool outputs
    userOutput                      <- findUserOutput swapRewardPkh swapRewardSPkh outputs
    let
      quote    = amountOf (fullTxOutValue userOutput) swapQuote
      execSwap = Interop.Swap
        { poolId      = wrap swapPoolId
        , baseIn      = swapBaseIn
        , minQuoteOut = swapMinQuoteOut
        , base        = wrap swapBase
        , quote       = wrap swapQuote
        , exFee       = swapExFee
        , rewardPkh   = swapRewardPkh
        , rewardSPkh  = swapRewardSPkh
        }
      order = ExecutedOrder
        { config       = execSwap
        , orderInputId = fullTxOutRef swapOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool

        }
    return 
      ExecutedSwap
        { swap        = order
        , actualQuote = quote
        }

data ExecutedDeposit = ExecutedDeposit
  { deposit  :: ExecutedOrder Interop.Deposit
  , rewardLq :: AssetAmountWrapper
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedDeposit where
  parseFromExplorer SettledTx{..} = do
    (OnChain depositOut deposit@Deposit{..}) <- findMatchedInput @Deposit inputs
    (OnChain prevPool Pool{..})              <- findMatchedInput @Pool inputs
    (OnChain currPool _)                     <- findMatchedOutput @Pool outputs
    userOutput                               <- findUserOutput depositRewardPkh depositRewardSPkh outputs
    let
      lqReward    = wrap $ assetAmountOfCoin (fullTxOutValue userOutput) poolCoinLq
      execDeposit = Interop.Deposit
        { poolId        = wrap depositPoolId
        , x             = wrap $ RIO.fst depositPair 
        , y             = wrap $ RIO.snd depositPair 
        , exFee         = depositExFee
        , rewardPkh     = depositRewardPkh
        , rewardSPkh    = depositRewardSPkh
        , adaCollateral = adaCollateral
        }
      order = ExecutedOrder
        { config       = execDeposit
        , orderInputId = fullTxOutRef depositOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool
        }
    return 
      ExecutedDeposit
        { deposit  = order
        , rewardLq = lqReward
        }

data ExecutedRedeem = ExecutedRedeem
  { redeem  :: ExecutedOrder Interop.Redeem
  , rewardX :: AssetAmountWrapper
  , rewardY :: AssetAmountWrapper
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer SettledTx ExecutedRedeem where
  parseFromExplorer SettledTx{..} = do
    (OnChain redeemOut redeem@Redeem{..}) <- findMatchedInput @Redeem inputs
    (OnChain prevPool Pool{..})           <- findMatchedInput @Pool inputs
    (OnChain currPool _)                  <- findMatchedOutput @Pool outputs
    userOutput                            <- findUserOutput redeemRewardPkh redeemRewardSPkh outputs
    let
      assetAmountX = wrap $ assetAmountOfCoin (fullTxOutValue userOutput) poolCoinX
      assetAmountY = wrap $ assetAmountOfCoin (fullTxOutValue userOutput) poolCoinY
      execRedeem   = Interop.Redeem
        { poolId     = wrap redeemPoolId
        , lqIn       = redeemLqIn
        , lq         = wrap redeemLq
        , exFee      = redeemExFee
        , rewardPkh  = redeemRewardPkh
        , rewardSPkh = redeemRewardSPkh
        }
      order = ExecutedOrder
        { config       = execRedeem
        , orderInputId = fullTxOutRef redeemOut
        , userOutputId = fullTxOutRef userOutput
        , poolOutputId = fullTxOutRef currPool
        , poolInputId  = fullTxOutRef prevPool
        }
    return 
      ExecutedRedeem
        { redeem  = order
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
