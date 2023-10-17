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
    { swapScriptPath    = "./scripts/swap.uplc"
    , depositScriptPath = "./scripts/deposit.uplc"
    , redeemScriptPath  = "./scripts/redeem.uplc"
    , poolScriptPath    = "./scripts/pool.uplc"
    },
  eventSourceConfig =
    { startAt =
        { slot = 105954179
        , hash = "add5b04e771f66893069dd42dd153d380378e56bb8fbc6bc7d2d854876036f09"
        }
    },
  nodeConfigPath = "/home/rawriclark/Cardano/mainnet-config.json",
  lederStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    },
  nodeSocketConfig =
    { nodeSocketPath = "./ipc/node.socket"
    , maxInFlight    = 256
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/tracker.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    }
}