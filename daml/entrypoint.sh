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

$API_SERVER \
  --replicas $REPLICAS \
  --participant-id $PARTICIPANT_ID --port 6865\
  --jdbc-url="jdbc:postgresql://$INDEXDB_HOST:$INDEXDB_PORT/$PARTICIPANT_ID?user=$INDEXDB_USER" \
  --maxInboundMessageSize=67108864 \
  $THIN_REPLICA_SETTINGS
