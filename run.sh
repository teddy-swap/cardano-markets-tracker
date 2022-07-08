docker run \
    -v /home/apps/tracker-haskell/conf/env.dhall:/etc/cardano-markets-tracker/config.dhall \
    --restart=always \
    --network="dev" \
    --name=htracker-haskell \
    -v /home/apps/tracker-haskell/logs:/cardano-markets-tracker/logs \
    -d timooxaaa/htracker-cardano:0.0.1
