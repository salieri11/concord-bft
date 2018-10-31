#!/bin/bash

PORT=42081
HTTP_PORT=42082
STORE_DIR=/tmp/helenTestDB
HOST=localhost

# clean if old directory exists
rm -rf ${STORE_DIR}

function cleanup_cockroach() {
    cockroach quit --insecure --host $HOST --port $PORT
    exit 0
}

# setup signal handler so that when python process sends SIGTERM we can kill cockroach DB process
trap "cleanup_cockroach" SIGTERM

cockroach start \
          --insecure \
          --background \
          --host=${HOST} \
          --http-port=${HTTP_PORT} \
          --port=${PORT} \
          --store=${STORE_DIR}


# Create a cockroach db user
cockroach user set --host ${HOST} --port ${PORT} helen_admin --insecure

# setup cockroach DB tables
cockroach sql --port ${PORT} --insecure < schema.sql

# Here we just need to wait infinitely until we receive SIGTERM, `read` is a perfect way
read
