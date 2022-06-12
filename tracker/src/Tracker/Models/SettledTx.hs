module Tracker.Models.SettledTx where

import qualified CardanoTx.Models as Tx

import Data.Aeson (FromJSON, ToJSON)

import Explorer.Class
import Explorer.Models
import Explorer.Types

import RIO

data SettledTx = SettledTx
  { blockHash   :: BlockHash
  , txIndex     :: Int
  , globalIndex :: Gix
  , hash        :: TxHash
  , inputs      :: [Tx.FullTxIn]
  , outputs     :: [Tx.FullTxOut]
  } deriving (Show, Eq, Generic, FromJSON)

mkFromExplorer :: FullTx -> SettledTx
mkFromExplorer FullTx{..} =
  SettledTx
    { blockHash   = blockHash
    , txIndex     = blockIndex
    , hash        = hash
    , globalIndex = globalIndex
    , inputs      = fmap toCardanoTx inputs
    , outputs     = fmap toCardanoTx outputs
    } 