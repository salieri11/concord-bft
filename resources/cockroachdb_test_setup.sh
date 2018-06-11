#!/bin/bash

PORT=42081
HTTP_PORT=42082
STORE_DIR=/tmp/helenTestDB
PID_FILE=/tmp/cockroachDB.pid
HOST=localhost
COCKROACH_PID=-1

# Print shell script pid
echo $$

# clean if old directory exists
rm -rf ${STORE_DIR}
rm ${PID_FILE}

function cleanup_cockroach() {
    echo "killing cockroachDB process at: ${COCKROACH_PID}"
    kill ${COCKROACH_PID}
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
          --pid-file=${PID_FILE} \
          --store=${STORE_DIR}


# Create a cockroach db user
cockroach user set --host ${HOST} --port ${PORT} helen_admin --insecure

# Create a helen database
cockroach sql --host ${HOST} --port ${PORT} --insecure -e 'CREATE DATABASE helen'

# Allow helen_admin all access to helen database
cockroach sql --host ${HOST} --port ${PORT} --insecure -e 'GRANT ALL ON DATABASE helen TO helen_admin'


# read pid file
COCKROACH_PID=`head -n 1 ${PID_FILE}`


echo "cockroach DB pid: ${COCKROACH_PID}"
# Here we just need to wait infinitely until we receive SIGTERM, `read` is a perfect way
read
