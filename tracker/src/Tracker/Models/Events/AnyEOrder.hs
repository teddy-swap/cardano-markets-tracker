module Tracker.Models.Events.AnyEOrder where

import Tracker.Models.ExecutedOrders
import Tracker.Models.Events.Class

import Data.Aeson
import RIO

data EOrder a where
  ESwap    :: ExecutedSwap    -> EOrder ExecutedSwap
  EDeposit :: ExecutedDeposit -> EOrder ExecutedDeposit
  ERedeem  :: ExecutedRedeem  -> EOrder ExecutedRedeem

data AnyEOrder = forall a . AnyEOrder
  { anyEOrder :: EOrder a
  }

instance ToJSON AnyEOrder where
  toJSON (AnyEOrder (ESwap order)) =
    object [ "ExecutedSwap" .= toJSON order
           ]
  toJSON (AnyEOrder (EDeposit order)) =
    object [ "ExecutedDeposit" .= toJSON order
           ]
  toJSON (AnyEOrder (ERedeem order)) =
    object [ "ExecutedRedeem" .= toJSON order
           ]

instance MakeAnyOrder ExecutedSwap AnyEOrder where
  make order =
    AnyEOrder $ ESwap order

instance MakeAnyOrder ExecutedDeposit AnyEOrder where
  make order =
    AnyEOrder $ EDeposit order  

instance MakeAnyOrder ExecutedRedeem AnyEOrder where
  make order =
    AnyEOrder $ ERedeem order