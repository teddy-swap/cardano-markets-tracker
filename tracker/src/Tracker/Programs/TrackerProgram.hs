module Tracker.Programs.TrackerProgram where

import Tracker.Services.Tracker
import Tracker.Caches.Cache

import Streaming.Events
import Streaming.Producer
import Streaming.Types

import qualified Streamly.Prelude as S
import           RIO

data TrackerProgram f = TrackerProgram
  { run :: f ()
  }

mkTrackerProgram
  :: (Monad i, S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> MakeLogging i f
  -> Tracker f
  -> Producer f String ExecutedOrderEvent
  -> i (TrackerProgram f)
mkTrackerProgram settings MakeLogging{..} explorer orderProd poolProd = do
  logger <- forComponent "trackerProgram"
  pure $ TrackerProgram $ run' settings logger explorer orderProd poolProd

run'
  :: (S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> Logging f
  -> Tracker f
  -> Producer f String ExecutedOrderEvent
  -> f ()
run' TrackerProgrammConfig{..} logging@Logging{..} explorer orderProd poolProd =
    S.repeatM (process explorer logging orderProd poolProd)
  & S.delay (fromIntegral $ Natural.naturalToInt pollTime)
  & S.handle (\(a :: SomeException) -> (lift . errorM $ ("tracker stream error: " ++ (show a)))) -- log.info here
  & S.drain

process
  :: (Monad f)
  => Tracker f
  -> Cache f
  -> Logging f
  -> Producer f String ExecutedOrderEvent
  -> f ()
process Tracker{..} Cache{..} Logging{..} producer = do
  transactions <- getAllTransactions
  let
    events =
        executedSwaps ++ executedDeposits ++ executedRedeems
      where
        executedSwaps    = ExecutedOrderEvent $ asKey $ parseFromExplorer transactions :: [ExecutedSwap]
        executedDeposits = ExecutedOrderEvent $ asKey $ parseFromExplorer transactions :: [ExecutedDeposit]
        executedRedeems  = ExecutedOrderEvent $ asKey $ parseFromExplorer transactions :: [ExecutedRedeem]
  _ <- infoM $ "Events are: "  ++ (show (length events))
  _ <- unless (null events) (produce producer (S.fromList events))
  