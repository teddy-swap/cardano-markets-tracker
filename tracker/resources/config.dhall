let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ txEventsProducerConfig =
    { producerBrokers = ["redpanda-1:19092"]
    , producerTimeout = 1000
    },
  ordersProducerConfig =
    { producerBrokers = ["redpanda-1:19092"]
    , producerTimeout = 1000
    },
  poolsProducerConfig =
    { producerBrokers = ["redpanda-1:19092"]
    , producerTimeout = 1000
    },
  mempoolOrdersProducerConfig =
    { producerBrokers = ["redpanda-1:19092"]
    , producerTimeout = 1000
    },  
  txEventsTopicName = "mainnet-tx-events",
  ordersTopicName = "mainnet-orders",
  poolsTopicName = "mainnet-lq-pools",
  mempoolOrdersTopicName = "mainnet-mempool-orders",
  scriptsConfig =
    { swapScriptPath    = "/scripts/swap.uplc"
    , depositScriptPath = "/scripts/deposit.uplc"
    , redeemScriptPath  = "/scripts/redeem.uplc"
    , poolScriptPath    = "/scripts/pool.uplc"
    },
  eventSourceConfig =
    { startAt =
        { slot = 109076993
        , hash = "328bac757d1b100c68e0fd8f346a1bd53ee415b94271b8b7353866a22063f7bf"
        }
    },
  nodeConfigPath = "/config/cardano/config.json",
  lederStoreConfig =
    { storePath       = "/data"
    , createIfMissing = True
    },
  nodeSocketConfig =
    { nodeSocketPath = "/ipc/node.socket"
    , maxInFlight    = 256
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "/dev/stdout" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    }
}