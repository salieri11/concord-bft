#!/bin/sh

# Stop execution if we have an error
set -e

# For development and testing, we set environment variables directly with docker
# or docker-compose options. However, for an automated deployment we need a more
# generic way to set these application specific variables.
CONFIG_FILE=/config/daml-index-db/environment-vars
if [ -e ${CONFIG_FILE} ]; then
  . ${CONFIG_FILE}
fi

echo "Running entry point script with $@ -c max_connections=${MAX_CONNECTIONS} -c shared_buffers=${BUFFER_SIZE}"
docker-entrypoint.sh $@ -c max_connections=${MAX_CONNECTIONS} -c shared_buffers=${BUFFER_SIZE}
