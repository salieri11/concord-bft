#!/bin/bash

# This Dockerfile builds a docker container with a single instance of
# CockroachDB running inside it.
# This container should be up and running before starting helen


docker run -d \
       --name=cockroach1 \
       --hostname=db-server \
       -p 26257:26257 -p 8081:8080  \
       -v "${PWD}/src/main/resources/database:/cockroach/resources" \
       -v "/tmp/cockroachDB:/cockroach/cockroach-data"  \
       cockroachdb/cockroach:v2.0.2 start --insecure


# Here we need to give sometime to the cockroachDB process to finish startup
# Maybe there is a better way to find out if setup is done (The --background
# option doens't work with docker)
sleep 7

docker exec -it cockroach1 ./cockroach user set helen_admin --insecure

# Setup database tables for profile management code
docker exec -it cockroach1 sh -c "./cockroach sql --insecure < /cockroach/resources/schema.sql"
