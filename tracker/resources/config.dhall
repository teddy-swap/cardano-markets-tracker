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
  txEventsTopicName = "tx-events",
  ordersTopicName = "orders-topic",
  poolsTopicName = "pools-topic-name",
  mempoolOrdersTopicName = "mempool-orders-topic",
  scriptsConfig =
    { swapScriptPath    = "/scripts/swap.uplc"
    , depositScriptPath = "/scripts/deposit.uplc"
    , redeemScriptPath  = "/scripts/redeem.uplc"
    , poolScriptPath    = "/scripts/pool.uplc"
    },
  eventSourceConfig =
    { startAt =
        { slot = 33096107
        , hash = "f836a3cbe4a1d944ef252c9d1bc56ae23a8f021dd8090529dc7d3d1fada09ce8"
        }
    },
  nodeConfigPath = "/config/cardano/config.json",
  lederStoreConfig =
    { storePath       = "/data/amm-executor"
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