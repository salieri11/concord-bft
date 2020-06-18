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

if [ "${USE_POSTGRES_CONFIG_FILE}" = "true" ]; then
  POSTGRES_CONFIG_FILE_OPT='-c config_file=/etc/postgresql.conf'
else
  MAX_CONNECTIONS_OPT="-c max_connections=${MAX_CONNECTIONS:-300}"
  BUFFER_SIZE_OPT="-c shared_buffers=${BUFFER_SIZE:-80MB}"
fi


echo "Running entry point script with $@ ${MAX_CONNECTIONS_OPT} ${BUFFER_SIZE_OPT} ${POSTGRES_CONFIG_FILE_OPT}"
docker-entrypoint.sh $@ ${MAX_CONNECTIONS_OPT} ${BUFFER_SIZE_OPT} ${POSTGRES_CONFIG_FILE_OPT}
