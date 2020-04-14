#!/bin/sh

# Stop execution if we have an error
set -e

# For development and testing, we set environment variables directly with docker
# or docker-compose options. However, for an automated deployment we need a more
# generic way to set these application specific variables.
CONFIG_FILE=/config/daml-ledger-api/environment-vars
if [ -e ${CONFIG_FILE} ]; then
  . ${CONFIG_FILE}
fi

API_SERVER=/doc/daml/ledger-api-server/target/universal/stage/bin/daml-on-vmware-ledger-api-server

# Check for --version and, if present, print the version and exit
while test $# -gt 0
do
  case "$1" in
    --version) $API_SERVER "$1" && exit 0
      ;;
  esac
  shift
done

# Try to connect to PostgreSQL
until psql -h "$INDEXDB_HOST" -p "$INDEXDB_PORT" -U "$INDEXDB_USER" -c '\q'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 5
done

>&2 echo "Postgres is up - starting ledger api server"

INDEXDB_JDBC_URL="jdbc:postgresql://$INDEXDB_HOST:$INDEXDB_PORT/$PARTICIPANT_ID?user=$INDEXDB_USER"

# Batching parameters. These are all overridable from the outside.
ENABLE_BATCHING=${ENABLE_BATCHING:=false}
MAX_BATCH_SIZE_BYTES=${MAX_BATCH_SIZE_BYTES:=$((4 * 1024 * 1024))} # "Soft" limit for batch size (default to 4MB)
MAX_BATCH_QUEUE_SIZE=${MAX_BATCH_QUEUE_SIZE:=100} # Max number of submissions to queue before dropping.
MAX_BATCH_WAIT_MILLIS=${MAX_BATCH_WAIT_MILLIS:=50} # Max amount of time to wait to construct a batch.
MAX_CONCURRENT_COMMITS=${MAX_CONCURRENT_COMMITS:=5} # Max number of concurrent commit calls towards concord.

$API_SERVER \
  --replicas $REPLICAS \
  --participant participant-id=$PARTICIPANT_ID,address=0.0.0.0,port=6865,server-jdbc-url="$INDEXDB_JDBC_URL" \
  --batching "enable=$ENABLE_BATCHING,max-queue-size=$MAX_BATCH_QUEUE_SIZE,max-batch-size-bytes=$MAX_BATCH_SIZE_BYTES,max-wait-millis=$MAX_BATCH_WAIT_MILLIS,max-concurrent-commits=$MAX_CONCURRENT_COMMITS" \
  --maxInboundMessageSize=67108864 \
  --ledger-id KVBC \
  $THIN_REPLICA_SETTINGS \
  $AUTH_SETTINGS
