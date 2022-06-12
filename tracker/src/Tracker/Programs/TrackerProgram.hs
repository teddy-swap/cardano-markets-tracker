module Tracker.Programs.TrackerProgram where

import Tracker.Services.Tracker
import Tracker.Caches.Cache
import Tracker.Models.AppConfig
import Tracker.Syntax.Option

import Streaming.Events
import Streaming.Producer
import Streaming.Types

import Tracker.Models.ExecutedOrders
import ErgoDex.Class

import Explorer.Class

import CardanoTx.Models

import System.Logging.Hlog
import GHC.Natural
import qualified Streamly.Prelude as S
import           RIO
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy as BS
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
  -> Producer f String ExecutedOrderEvent
  -> i (TrackerProgram f)
mkTrackerProgram settings MakeLogging{..} cache tracker producer = do
  logger <- forComponent "trackerProgram"
  pure $ TrackerProgram $ run' settings logger cache tracker producer

run'
  :: (S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> Logging f
  -> Cache f
  -> TrackerService f
  -> Producer f String ExecutedOrderEvent
  -> f ()
run' TrackerProgrammConfig{..} logging@Logging{..} cache service producer =
    S.repeatM (process service cache logging producer)
  & S.delay (fromIntegral $ naturalToInt pollTime)
  & S.handle (\(a :: SomeException) -> (lift . errorM $ ("tracker stream error: " ++ (show a)))) -- log.info here
  & S.drain

process
  :: (Monad f)
  => TrackerService f
  -> Cache f
  -> Logging f
  -> Producer f String ExecutedOrderEvent
  -> f ()
process TrackerService{..} Cache{..} Logging{..} producer = do
  (transactions, index) <- getAllTransactions
  let
    events =
        executedSwaps ++ executedDeposits ++ executedRedeems
      where
        executedSwaps    = processExecutedOrder @ExecutedSwap  transactions
        executedDeposits = processExecutedOrder @ExecutedDeposit transactions
        executedRedeems  = processExecutedOrder @ExecutedRedeem transactions
  _ <- infoM $ "Events are: "  ++ (show (length events))
  _ <- unless (null events) (produce producer (S.fromList events))
  putLastIndex index

constantKafkaKey :: String
constantKafkaKey = "kafka_key"

processExecutedOrder :: forall b. (FromExplorer SettledTx b, ToJSON b) => [SettledTx] -> [(String, ExecutedOrderEvent)]
processExecutedOrder inputs =
  let 
    ordersMaybe = 
      fmap (\elem ->
        case parseFromExplorer elem :: Maybe b of
          Just order -> Just $ (constantKafkaKey, ExecutedOrderEvent $ BS.toStrict $ encode order)
          _          -> Nothing
        ) inputs
  in unNone [] ordersMaybe
    