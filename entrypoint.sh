#!/bin/bash

mkdir -p /config

# Generate the config
bash ./generate-config.sh

# Move the generated config to the desired location
mv config.dhall /config/tracker.dhall

# Execute the tracker-app with the new config
exec /usr/local/bin/tracker-app /config/tracker.dhall "$@"