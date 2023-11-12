{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Tracker.App
  ( App (..),
    runApp,
  )
where

import Cardano.Api (SlotNo (unSlotNo, SlotNo))
import Cardano.Crypto.Hashing (decodeHash, hashToBytes)
import Cardano.Network.Protocol.NodeToClient.Trace
  ( encodeTraceClient,
  )
import CardanoTx.Models
  ( FullTxOut (..),
  )
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TVar as STM
import Control.Monad.Base
  ( MonadBase,
  )
import qualified Control.Monad.Catch as MC
import Control.Monad.Class.MonadAsync
  ( MonadAsync (..),
  )
import Control.Monad.Class.MonadFork
  ( MonadFork,
    MonadThread,
  )
import Control.Monad.Class.MonadST
  ( MonadST,
  )
import Control.Monad.Class.MonadSTM
  ( MonadSTM (..),
  )
import Control.Monad.Class.MonadThrow
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import qualified Control.Monad.STM as STM
import Control.Monad.Trans.Class
  ( MonadTrans (lift),
  )
import Control.Monad.Trans.Control
  ( MonadBaseControl,
  )
import Control.Monad.Trans.Resource
  ( MonadUnliftIO,
    ResourceT,
    runResourceT,
  )
import Control.Tracer
  ( Contravariant (contramap),
    stdoutTracer,
  )
import Crypto.Random.Entropy
  ( getEntropy,
  )
import Crypto.Random.Types
  ( MonadRandom (..),
  )
import Data.Aeson
  ( encode,
  )
import Data.ByteString.Lazy.UTF8
  ( toString,
  )
import Data.ByteString.Short (toShort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Dhall
  ( Generic,
    Text,
  )
import ErgoDex.Amm.Orders
  ( AnyOrder (AnyOrder),
    Deposit (..),
    OrderAction (..),
    Redeem (..),
    Swap (..),
  )
import ErgoDex.Amm.Pool
  ( Pool,
  )
import ErgoDex.Class
  ( FromLedger (..),
  )
import ErgoDex.ScriptsValidators
  ( ScriptsConfig,
    ScriptsValidators,
    mkScriptsValidators,
    parsePool,
  )
import ErgoDex.State
  ( Confirmed (..),
    OnChain (..),
  )
import Kafka.Producer
  ( TopicName (TopicName),
  )
import qualified Ledger as P
import Ouroboros.Consensus.Cardano.Block
  ( CardanoEras,
  )
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (OneEraHash))
import RIO
  ( Alternative (..),
    MonadIO (liftIO),
    MonadPlus (..),
    MonadReader (ask),
    ReaderT (..),
    void,
    (<&>),
  )
import RIO.List
  ( headMaybe,
  )
import Spectrum.Config
  ( EventSourceConfig (startAt),
  )
import Spectrum.EventSource.Data.Tx
  ( MinimalConfirmedTx (..),
    MinimalTx (..),
    MinimalUnconfirmedTx (..),
  )
import Spectrum.EventSource.Data.TxContext
  ( TxCtx (LedgerCtx, MempoolCtx),
  )
import Spectrum.EventSource.Data.TxEvent
  ( TxEvent (..),
  )
import Spectrum.EventSource.Persistence.Config (LedgerStoreConfig, storePath)
import Spectrum.EventSource.Stream
  ( EventSource (..),
    mkLedgerEventSource,
    mkMempoolTxEventSource,
  )
import Spectrum.EventSource.Types
  ( ConcreteHash (ConcreteHash),
    ConcretePoint (ConcretePoint),
  )
import Spectrum.LedgerSync
  ( LedgerSync,
    mkLedgerSync,
  )
import Spectrum.LedgerSync.Config
  ( NetworkParameters,
    NodeSocketConfig,
    parseNetworkParameters,
  )
import Streaming.Config
  ( KafkaProducerConfig (..),
  )
import Streaming.Producer
  ( Producer (produce),
    mkKafkaProducer,
  )
import Streamly.Prelude as S
  ( IsStream,
    SerialT,
    drain,
    fromList,
    mapM,
    parallel,
  )
import System.Directory (doesFileExist)
import System.Logging.Hlog
  ( Logging (..),
    MakeLogging (forComponent),
    makeLogging,
    translateMakeLogging,
  )
import System.Posix.Signals
  ( Handler (..),
    installHandler,
    keyboardSignal,
    raiseSignal,
    softwareTermination,
  )
import Tracker.Models.AppConfig
import Tracker.Models.OnChainEvent (OnChainEvent (OnChainEvent))
import Tracker.Syntax.Option
  ( unNone,
  )
import Data.Word (Word64)

newtype App a = App
  { unApp :: ReaderT (Env Wire App) IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env Wire App),
      MonadIO,
      MonadST,
      MonadThread,
      MonadFork,
      MonadThrow,
      MC.MonadThrow,
      MonadCatch,
      MC.MonadCatch,
      MonadMask,
      MC.MonadMask,
      MonadBase IO,
      MonadBaseControl IO,
      MonadUnliftIO
    )

type Wire = ResourceT App

data Env f m = Env
  { nodeSocketConfig :: !NodeSocketConfig,
    ledgerStoreConfig :: !LedgerStoreConfig,
    eventSourceConfig :: !EventSourceConfig,
    networkParams :: !NetworkParameters,
    txEventsProducerConfig :: !KafkaProducerConfig,
    txEventsTopicName :: !Text,
    ordersProducerConfig :: !KafkaProducerConfig,
    ordersTopicName :: !Text,
    mempoolOrdersProducerConfig :: !KafkaProducerConfig,
    mempoolOrdersTopicName :: !Text,
    poolsProducerConfig :: !KafkaProducerConfig,
    poolsTopicName :: !Text,
    scriptsConfig :: !ScriptsConfig,
    mkLogging :: !(MakeLogging f m),
    mkLogging' :: !(MakeLogging m m)
  }
  deriving stock (Generic)

runContext :: Env Wire App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

runApp :: [String] -> IO ()
runApp args = do
  AppConfig {..} <- loadAppConfig (headMaybe args)
  nparams <- parseNetworkParameters nodeConfigPath
  mkLogging <- makeLogging loggingConfig :: IO (MakeLogging IO App)

  let cursorFilePath = storePath lederStoreConfig ++ "/cursor"
  fileExists <- doesFileExist cursorFilePath
  updatedEventSourceConfig <- if fileExists
    then do
      cursorContents <- TIO.readFile cursorFilePath
      let (slotStr, hashStr) = T.break (== ',') cursorContents
      let slotWord64 = read $ T.unpack $ T.takeWhile (/= ',') slotStr :: Word64
      let slot = SlotNo slotWord64
      let hashText = T.drop 1 hashStr
      case oneEraHashFromString hashText of
        Left errMsg -> fail errMsg  -- Fixed line
        Right hh -> return eventSourceConfig { startAt = ConcretePoint slot (ConcreteHash hh) }
    else return eventSourceConfig

  let env =
        Env
          nodeSocketConfig
          lederStoreConfig
          updatedEventSourceConfig
          nparams
          txEventsProducerConfig
          txEventsTopicName
          ordersProducerConfig
          ordersTopicName
          mempoolOrdersProducerConfig
          mempoolOrdersTopicName
          poolsProducerConfig
          poolsTopicName
          scriptsConfig
          (translateMakeLogging (lift . App . lift) mkLogging)
          (translateMakeLogging (App . lift) mkLogging)
  runContext env (runResourceT wireApp)

wireApp :: Wire ()
wireApp = do
  env@Env {..} <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  let ledgerStorePath = storePath ledgerStoreConfig
  let cursorFilePath = ledgerStorePath ++ "/cursor"
  
  lsync <- lift $ mkLedgerSync (runContext env) tr mkLogging' nodeSocketConfig networkParams -- :: ResourceT App (LedgerSync App)
  lsource <- mkLedgerEventSource lsync lift :: ResourceT App (EventSource S.SerialT App 'LedgerCtx)
  msource <- mkMempoolTxEventSource lsync :: ResourceT App (EventSource S.SerialT App 'MempoolCtx)

  scriptsValidators <- lift $ mkScriptsValidators scriptsConfig
  processTxEventsLogging <- forComponent mkLogging "processTxEvents"

  txEventsProducer <- mkKafkaProducer txEventsProducerConfig (TopicName txEventsTopicName)
  ordersProducer <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  mempoolOrdersProducer <- mkKafkaProducer mempoolOrdersProducerConfig (TopicName mempoolOrdersTopicName)
  poolsProducer <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  lift . S.drain $
    S.parallel (processTxEvents processTxEventsLogging scriptsValidators (upstream lsource) txEventsProducer ordersProducer poolsProducer cursorFilePath) $
      processMempoolTxEvents processTxEventsLogging (upstream msource) mempoolOrdersProducer

processTxEvents ::
  ( IsStream s,
    MonadIO m,
    MonadBaseControl IO m,
    MC.MonadThrow m
  ) =>
  Logging m ->
  ScriptsValidators ->
  s m (TxEvent ctx) ->
  Producer m String (TxEvent ctx) ->
  Producer m String (OnChainEvent AnyOrder) ->
  Producer m String (OnChainEvent Pool) ->
  FilePath ->
  s m ()
processTxEvents logging scriptsValidators txEventsStream txEventsProducer ordersProducer poolProducer cursorFilePath =
  S.mapM
    ( \txEvent -> do
        produce txEventsProducer (S.fromList [(mkKafkaKey txEvent, txEvent)])
        parseOrders logging txEvent >>= write2Kafka ordersProducer
        parsePools logging scriptsValidators txEvent >>= write2Kafka poolProducer

        -- Extract slot and hash from TxEvent and update the cursor file
        let (slot, maybeHash) = extractSlotAndHash txEvent
        liftIO $ updateCursorFile cursorFilePath slot maybeHash
    )
    txEventsStream

processMempoolTxEvents ::
  ( IsStream s,
    MonadIO m,
    MonadBaseControl IO m,
    MC.MonadThrow m
  ) =>
  Logging m ->
  s m (TxEvent ctx) ->
  Producer m String (OnChainEvent AnyOrder) ->
  s m ()
processMempoolTxEvents logging txEventsStream mempoolOrdersProducer =
  S.mapM
    ( \txEvent -> do
        parseOrders logging txEvent >>= write2Kafka mempoolOrdersProducer
    )
    txEventsStream

updateCursorFile :: FilePath -> SlotNo -> Maybe P.BlockId -> IO ()
updateCursorFile filePath slotNo maybeBlockId = do
  let slotNoStr = show $ unSlotNo slotNo -- Extract the numerical value
  let maybeHashStr = maybe "" show maybeBlockId
  let slotHashString = slotNoStr ++ "," ++ maybeHashStr
  writeFile filePath slotHashString

extractSlotAndHash :: TxEvent ctx -> (SlotNo, Maybe P.BlockId)
extractSlotAndHash (PendingTx (MinimalMempoolTx minTx)) =
  let MinimalUnconfirmedTx {slotNo = slot} = minTx
   in (slot, Nothing)
extractSlotAndHash (AppliedTx (MinimalLedgerTx minTx)) =
  let MinimalConfirmedTx {slotNo = slot, blockId = hash} = minTx
   in (slot, Just hash)
extractSlotAndHash _ =
  (0, Nothing) -- Handle other cases as appropriate

oneEraHashFromString :: T.Text -> Either String (OneEraHash (CardanoEras crypto))
oneEraHashFromString =
  either (const $ Left "Invalid hash") (Right . OneEraHash . toShort . hashToBytes) . decodeHash

write2Kafka :: (Monad m) => Producer m String (OnChainEvent a) -> [OnChainEvent a] -> m ()
write2Kafka producer = produce producer . S.fromList . mkKafkaTuple

parsePools :: forall m ctx. (MonadIO m) => Logging m -> ScriptsValidators -> TxEvent ctx -> m [OnChainEvent Pool]
parsePools logging scriptsValidators (AppliedTx (MinimalLedgerTx MinimalConfirmedTx {..})) =
  (parsePool logging scriptsValidators `traverse` txOutputs) <&> unNone <&> (\confirmedList -> (\(Confirmed _ a) -> OnChainEvent a slotNo) <$> confirmedList)
parsePools _ _ _ = pure []

mkKafkaTuple :: [OnChainEvent a] -> [(String, OnChainEvent a)]
mkKafkaTuple ordersList = (\event@(OnChainEvent (OnChain FullTxOut {..} _) _) -> (show fullTxOutRef, event)) <$> ordersList

parseOrders :: forall m ctx. (MonadIO m) => Logging m -> TxEvent ctx -> m [OnChainEvent AnyOrder]
parseOrders logging (AppliedTx (MinimalLedgerTx MinimalConfirmedTx {..})) =
  (parseOrder logging slotNo `traverse` txOutputs) <&> unNone
parseOrders logging (PendingTx (MinimalMempoolTx MinimalUnconfirmedTx {..})) =
  (parseOrder logging slotNo `traverse` txOutputs) <&> unNone
parseOrders _ _ = pure []

parseOrder :: (MonadIO m) => Logging m -> SlotNo -> FullTxOut -> m (Maybe (OnChainEvent AnyOrder))
parseOrder Logging {..} slot out =
  let swap = parseFromLedger @Swap out
      deposit = parseFromLedger @Deposit out
      redeem = parseFromLedger @Redeem out
   in case (swap, deposit, redeem) of
        (Just (OnChain _ swap'), _, _) -> do
          infoM ("Swap order: " ++ show swap)
          pure . Just $ OnChainEvent (OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')) slot
        (_, Just (OnChain _ deposit'), _) -> do
          infoM ("Deposit order: " ++ show deposit)
          pure . Just $ OnChainEvent (OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')) slot
        (_, _, Just (OnChain _ redeem')) -> do
          infoM ("Redeem order: " ++ show redeem)
          pure . Just $ OnChainEvent (OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')) slot
        _ -> do
          infoM ("Order not found in: " ++ show (fullTxOutRef out))
          pure Nothing

mkKafkaKey :: TxEvent ctx -> String
mkKafkaKey (PendingTx (MinimalMempoolTx MinimalUnconfirmedTx {..})) = show txId
mkKafkaKey (AppliedTx (MinimalLedgerTx MinimalConfirmedTx {..})) = show txId
mkKafkaKey (UnappliedTx txId) = show txId

instance MonadRandom App where
  getRandomBytes = liftIO . getEntropy

newtype WrappedSTM a = WrappedSTM {unwrapSTM :: STM.STM a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM App where
  type STM App = WrappedSTM
  type TVar App = STM.TVar
  type TMVar App = STM.TMVar
  type TQueue App = STM.TQueue
  type TBQueue App = STM.TBQueue

  atomically = App . lift . STM.atomically . unwrapSTM
  retry = WrappedSTM STM.retry
  orElse = \a0 a1 -> WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check = WrappedSTM . STM.check

  newTVar = WrappedSTM . STM.newTVar
  newTVarIO = App . lift . STM.newTVarIO
  readTVar = WrappedSTM . STM.readTVar
  readTVarIO = App . lift . STM.readTVarIO
  writeTVar = \a0 -> WrappedSTM . STM.writeTVar a0
  modifyTVar = \a0 -> WrappedSTM . STM.modifyTVar a0
  modifyTVar' = \a0 -> WrappedSTM . STM.modifyTVar' a0
  stateTVar = \a0 -> WrappedSTM . STM.stateTVar a0
  swapTVar = \a0 -> WrappedSTM . STM.swapTVar a0

  newTMVar = WrappedSTM . STM.newTMVar
  newTMVarIO = App . lift . STM.newTMVarIO
  newEmptyTMVar = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = App (lift STM.newEmptyTMVarIO)
  takeTMVar = WrappedSTM . STM.takeTMVar
  tryTakeTMVar = WrappedSTM . STM.tryTakeTMVar
  putTMVar = \a0 -> WrappedSTM . STM.putTMVar a0
  tryPutTMVar = \a0 -> WrappedSTM . STM.tryPutTMVar a0
  readTMVar = WrappedSTM . STM.readTMVar
  tryReadTMVar = WrappedSTM . STM.tryReadTMVar
  swapTMVar = \a0 -> WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar = WrappedSTM . STM.isEmptyTMVar

  newTQueue = WrappedSTM STM.newTQueue
  newTQueueIO = App (lift STM.newTQueueIO)
  readTQueue = WrappedSTM . STM.readTQueue
  tryReadTQueue = WrappedSTM . STM.tryReadTQueue
  peekTQueue = WrappedSTM . STM.peekTQueue
  tryPeekTQueue = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue = WrappedSTM . STM.flushTBQueue
  writeTQueue = \a0 -> WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue = WrappedSTM . STM.isEmptyTQueue

  newTBQueue = WrappedSTM . STM.newTBQueue
  newTBQueueIO = App . lift . STM.newTBQueueIO
  readTBQueue = WrappedSTM . STM.readTBQueue
  tryReadTBQueue = WrappedSTM . STM.tryReadTBQueue
  peekTBQueue = WrappedSTM . STM.peekTBQueue
  tryPeekTBQueue = WrappedSTM . STM.tryPeekTBQueue
  writeTBQueue = \a0 -> WrappedSTM . STM.writeTBQueue a0
  lengthTBQueue = WrappedSTM . STM.lengthTBQueue
  isEmptyTBQueue = WrappedSTM . STM.isEmptyTBQueue
  isFullTBQueue = WrappedSTM . STM.isFullTBQueue

newtype WrappedAsync a = WrappedAsync {unwrapAsync :: Async.Async a}
  deriving newtype (Functor)

instance MonadAsync App where
  type Async App = WrappedAsync
  async = \(App (ReaderT m)) -> App (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncThreadId = Async.asyncThreadId . unwrapAsync
  pollSTM = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel = App . lift . Async.cancel . unwrapAsync
  cancelWith = \a0 -> App . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask = \restore -> App $
    ReaderT $ \r ->
      fmap WrappedAsync $
        Async.asyncWithUnmask $ \unmask ->
          runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))