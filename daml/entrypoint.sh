#!/bin/sh

# Stop execution if we have an error
set -e

# Try to connect to PostgreSQL
until psql -h "$INDEXDB_HOST" -p "$INDEXDB_PORT" -U "$INDEXDB_USER" -c '\q'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 3
done

>&2 echo "Postgres is up - starting ledger api server"

/doc/daml/kvbc_ledger_server/target/universal/stage/bin/kvbc-ledger-server $CONCORD_HOST:$CONCORD_PORT $SELF_NAME

