#!/bin/sh

# Stop execution if we have an error
set -e

# For development and testing, we set environment variables directly with docker
# or docker-compose options. However, for an automated deployment we need a more
# generic way to set these application specific variables.
CONFIG_FILE=/config/daml-execution-engine/environment-vars
if [ -e ${CONFIG_FILE} ]; then
  . ${CONFIG_FILE}
fi

LOGBACK_CONFIG_FILE="/doc/daml/execution-engine/resources/logback.xml"

/doc/daml/execution-engine/target/universal/stage/bin/daml-on-vmware-execution-engine \
  -Dlogback.configurationFile=$LOGBACK_CONFIG_FILE \
  $VALIDATION_SETTINGS \
  $@
