#!/bin/sh

# Stop execution if we have an error
set -e

# Try to connect to PostgreSQL
until psql -h "$INDEXDB_HOST" -p "$INDEXDB_PORT" -U "$INDEXDB_USER" -c '\q'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 5
done

>&2 echo "Postgres is up - starting ledger api server"

/doc/daml/kvbc_ledger_server/target/universal/stage/bin/kvbc-ledger-server \
  --replica-host $CONCORD_HOST --replica-port $CONCORD_PORT \
  --participant-id $PARTICIPANT_ID --port 6865\
  --jdbc-url="jdbc:postgresql://$INDEXDB_HOST:$INDEXDB_PORT/$PARTICIPANT_ID?user=$INDEXDB_USER" \
  --maxInboundMessageSize=67108864
