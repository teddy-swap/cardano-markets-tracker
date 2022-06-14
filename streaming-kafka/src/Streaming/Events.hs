module Streaming.Events
  ( ExecutedOrderEvent(..)
  , asKey
  ) where

import Data.Aeson
import GHC.Generics
import Prelude 
import Kafka.Producer
import Kafka.Consumer
import RIO

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import CardanoTx.Models 
import Streaming.Class
import Explorer.Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as ByteString

data ExecutedOrderEvent = ExecutedOrderEvent
  { stringJson :: String
  } deriving (Generic, FromJSON, ToJSON)

instance ToKafka String ExecutedOrderEvent where
  toKafka topic k v =
      ProducerRecord topic UnassignedPartition encodedKey encodedValue
    where
      encodedValue = asKey v
      encodedKey   = asKey k

asKey :: (ToJSON a) => a -> Maybe ByteString.ByteString
asKey = Just . BS.toStrict . encode

fromKey :: (FromJSON a) => Maybe ByteString.ByteString -> Maybe a
fromKey (Just bs) = (decode . BS.fromStrict) bs
fromKey _         = Nothing