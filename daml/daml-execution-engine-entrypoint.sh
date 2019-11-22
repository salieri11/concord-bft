#!/bin/sh

# Stop execution if we have an error
set -e

# For development and testing, we set environment variables directly with docker
# or docker-compose options. However, for an automated deployment we need a more
# generic way to set these application specific variables.
CONFIG_FILE=/config/daml-execution-engine/environment-vars
if [ -e ${CONFIG_FILE} ]; then
  source ${CONFIG_FILE}
fi

/doc/daml/kvbc_validator/target/universal/stage/bin/kvbc-validator