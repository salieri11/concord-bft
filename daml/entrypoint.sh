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
DATE_CMD="date --iso-8601=seconds | awk -F'+' '{print \$1}'"

# Check for --version and, if present, print the version and exit
while test $# -gt 0
do
  case "$1" in
    --version) $API_SERVER "$1" && exit 0
      ;;
  esac
  shift
done

# Coredump location
sysctl kernel.core_pattern=/config/daml-ledger-api/cores/core.%e.%h.%s.%t >/dev/null

# Try to connect to PostgreSQL
until psql -h "$INDEXDB_HOST" -p "$INDEXDB_PORT" -U "$INDEXDB_USER" -c '\q'; do
  >&2 echo $(eval $DATE_CMD) "Postgres is unavailable - sleeping"
  sleep 5
done

>&2 echo $(eval $DATE_CMD) "Postgres is up - starting ledger api server"

INDEXDB_JDBC_URL="jdbc:postgresql://$INDEXDB_HOST:$INDEXDB_PORT/$INDEX_DB_SCHEMA?user=$INDEXDB_USER"

# Pre-execution cost threshold parameter.
if [ -z "$PRE_EXECUTION_COST_THRESHOLD" ]; then
  echo "Disabling pre-execution"
  PRE_EXECUTION_COST_THRESHOLD_PARAMETER=""
else
  echo "Enabling pre-execution with cost threshold $PRE_EXECUTION_COST_THRESHOLD"
  PRE_EXECUTION_COST_THRESHOLD_PARAMETER="--pre-execution-cost-threshold $PRE_EXECUTION_COST_THRESHOLD"
fi

# Batching parameters. These are all overridable from the outside.
ENABLE_BATCHING=${ENABLE_BATCHING:=true}
MAX_BATCH_SIZE_BYTES=${MAX_BATCH_SIZE_BYTES:=$((4 * 1024 * 1024))} # "Soft" limit for batch size (default to 4MB)
MAX_BATCH_QUEUE_SIZE=${MAX_BATCH_QUEUE_SIZE:=100} # Max number of submissions to queue before dropping.
MAX_BATCH_WAIT_MILLIS=${MAX_BATCH_WAIT_MILLIS:=50} # Max amount of time to wait to construct a batch.
MAX_CONCURRENT_COMMITS=${MAX_CONCURRENT_COMMITS:=5} # Max number of concurrent commit calls towards concord.

# We assume n = 3*f + 2*c + 1 whereby c=0
N=$(echo $REPLICAS | awk -F"," '{print NF}')
MAX_FAULTY_REPLICAS=${MAX_FAULTY_REPLICAS:=$((($N - 1) / 3))}

# Timeout in seconds per read request
MAX_TRC_READ_DATA_TIMEOUT=${MAX_TRC_READ_DATA_TIMEOUT:=3}
MAX_TRC_READ_HASH_TIMEOUT=${MAX_TRC_READ_HASH_TIMEOUT:=3}

LOGBACK_CONFIG_FILE="/doc/daml/ledger-api-server/resources/logback.xml"

$API_SERVER \
  -Dlogback.configurationFile=$LOGBACK_CONFIG_FILE \
  --replicas $REPLICAS \
  --participant participant-id=$PARTICIPANT_ID,address=0.0.0.0,port=6865,server-jdbc-url="$INDEXDB_JDBC_URL" \
  --batching "enable=$ENABLE_BATCHING,max-queue-size=$MAX_BATCH_QUEUE_SIZE,max-batch-size-bytes=$MAX_BATCH_SIZE_BYTES,max-wait-millis=$MAX_BATCH_WAIT_MILLIS,max-concurrent-commits=$MAX_CONCURRENT_COMMITS" \
  --max-inbound-message-size 67108864 \
  --maxFaultyReplicas ${MAX_FAULTY_REPLICAS} \
  --maxTrcReadDataTimeout ${MAX_TRC_READ_DATA_TIMEOUT} \
  --maxTrcReadHashTimeout ${MAX_TRC_READ_HASH_TIMEOUT} \
  --ledger-id KVBC \
  $PRE_EXECUTION_COST_THRESHOLD_PARAMETER \
  $THIN_REPLICA_SETTINGS \
  $BFT_CLIENT_SETTINGS \
  $AUTH_SETTINGS
