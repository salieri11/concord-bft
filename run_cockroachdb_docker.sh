#!/bin/bash

# This Dockerfile builds a docker container with a single instance of
# CockroachDB running inside it.
# This container should be up and running before starting helen


docker run -d \
       --name=cockroach1 \
       --hostname=db-server \
       -p 26257:26257 -p 8081:8080  \
       -v "/tmp/cockroachDB:/cockroach/cockroach-data"  \
       cockroachdb/cockroach:v2.0.2 start --insecure

# Here we need to give sometime to the cockroachDB process to finish startup
# Maybe there is a better way to find out if setup is done (The --background
# option doens't work with docker)
sleep 10

docker exec -it cockroach1 ./cockroach user set helen_admin --insecure

# Create a helen database

docker exec -it cockroach1 ./cockroach sql --insecure -e 'CREATE DATABASE IF NOT EXISTS helen'


# Allow helen_admin all access to helen database
docker exec -it cockroach1 ./cockroach sql --insecure -e 'GRANT ALL ON DATABASE helen TO helen_admin'


## Run image
# FROM cockroachdb/cockroach:v2.0.2
# LABEL Description="cockroachdb"
# RUN start

# EXPOSE 26257
# EXPOSE 8080

# WORKDIR /
# COPY priv priv
# COPY config.properties .
# COPY helen*.jar .

# prepare for docker-compose, where athena is available from a virtual
# host named "athena"
# RUN sed -i -e "s/AthenaHostName=.*/AthenaHostName=athena/g" config.properties

# CMD java -jar helen*.jar

# EXPOSE 8080
