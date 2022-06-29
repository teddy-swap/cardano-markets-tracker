module Tracker.Programs.TrackerProgram where

import Tracker.Services.Tracker
import Tracker.Caches.Cache
import Tracker.Models.AppConfig
import Tracker.Syntax.Option
import Tracker.Models.ExecutedOrders
import Tracker.Models.SettledTx
import qualified Tracker.Models.Interop.Pool as Interop
import Tracker.Models.Events.PoolEvent
import Tracker.Models.Interop.Class
import Tracker.Models.Events.AnyEOrder
import Tracker.Models.Events.Class

import Streaming.Producer
import Streaming.Types

import ErgoDex.Class
import ErgoDex.Amm.Pool
import ErgoDex.State

import Explorer.Class

import CardanoTx.Models

import System.Logging.Hlog
import GHC.Natural
import qualified Streamly.Prelude as S
import           RIO
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson

data TrackerProgram f = TrackerProgram
  { run :: f ()
  }

mkTrackerProgram
  :: (Monad i, S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> MakeLogging i f
  -> Cache f
  -> TrackerService f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> i (TrackerProgram f)
mkTrackerProgram settings MakeLogging{..} cache tracker executedOrdersProducer poolsProducer = do
  logger <- forComponent "trackerProgram"
  pure $ TrackerProgram $ run' settings logger cache tracker executedOrdersProducer poolsProducer

run'
  :: (S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> Logging f
  -> Cache f
  -> TrackerService f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> f ()
run' TrackerProgrammConfig{..} logging@Logging{..} cache service executedOrdersProducer poolsProducer =
    S.repeatM (process service cache logging executedOrdersProducer poolsProducer)
  & S.delay (fromIntegral $ naturalToInt pollTime)
  & S.handle (\(a :: SomeException) -> (lift . errorM $ ("tracker stream error: " ++ (show a)))) -- log.info here
  & S.drain

process
  :: (Monad f)
  => TrackerService f
  -> Cache f
  -> Logging f
  -> Producer f String AnyEOrder
  -> Producer f String PoolEvent
  -> f ()
process TrackerService{..} Cache{..} Logging{..} executedOrdersProducer poolsProducer = do
  (transactions, index) <- getAllTransactions
  let
    events =
        executedSwaps ++ executedDeposits ++ executedRedeems
      where
        executedSwaps    = processExecutedOrder @ExecutedSwap  transactions
        executedDeposits = processExecutedOrder @ExecutedDeposit transactions
        executedRedeems  = processExecutedOrder @ExecutedRedeem transactions
    pools = processPool transactions
  _ <- infoM $ "Events are: "  ++ (show (length events))
  _ <- unless (null events) (produce executedOrdersProducer (S.fromList events))
  _ <- infoM $ "Pools are: "  ++ (show (length pools))
  _ <- unless (null pools) (produce poolsProducer (S.fromList pools))
  putLastIndex index

constantKafkaKey :: String
constantKafkaKey = "kafka_key"

processExecutedOrder :: forall b. (FromExplorer SettledTx b, MakeAnyOrder b AnyEOrder) => [SettledTx] -> [(String, AnyEOrder)]
processExecutedOrder inputs =
  let 
    ordersMaybe = 
      fmap (\elem ->
        case parseFromExplorer elem :: Maybe b of
          Just order -> Just $ (constantKafkaKey, make order)
          _          -> Nothing
        ) inputs
  in unNone ordersMaybe

processPool :: [SettledTx] -> [(String, PoolEvent)]
processPool transactions =
  let
    poolsMaybe = transactions >>= (\SettledTx{outputs, timestamp} ->
        fmap (\out -> case parseFromLedger @Pool out of
            Just (OnChain out Pool{..}) ->
              let
                pool = Interop.Pool
                  { id            = wrap poolId
                  , reservesX     = poolReservesX
                  , reservesY     = poolReservesY
                  , liquidity     = poolLiquidity 
                  , x             = wrap poolCoinX 
                  , y             = wrap poolCoinY 
                  , lq            = wrap poolCoinLq 
                  , fee           = wrap poolFee 
                  , outCollateral = outCollateral
                  } 
              in Just ((show $ poolId), PoolEvent pool timestamp (fullTxOutRef out))
            _ -> Nothing
          ) outputs
      )
  in unNone poolsMaybe
