let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ txEventsProducerConfig =
    { producerBrokers = ["192.168.50.25:19092"]
    , producerTimeout = 1000
    },
  ordersProducerConfig =
    { producerBrokers = ["192.168.50.25:19092"]
    , producerTimeout = 1000
    },
  poolsProducerConfig =
    { producerBrokers = ["192.168.50.25:19092"]
    , producerTimeout = 1000
    },
  mempoolOrdersProducerConfig =
    { producerBrokers = ["192.168.50.25:19092"]
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
        { slot = 31425678
        , hash = "3b6afd2b4c7827137455a70616ab1e3d90083055fa97b4be867f1e2ea4e1a04d"
        }
    },
  nodeConfigPath = "/config/cardano/config.json",
  lederStoreConfig =
    { storePath       = "/data/amm-executor"
    , createIfMissing = True
    },
  nodeSocketConfig =
    { nodeSocketPath = "/tmp/node.socket"
    , maxInFlight    = 256
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "/dev/stdout" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    }
}