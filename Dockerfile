# This Dockerfile does not build Helen, it just dockerizes it.
# You must build the UI and the HTTP Server first (see Readme.md).
# Also, helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.

## Run image
FROM ubuntu:16.04
LABEL Description="Helen"

RUN apt-get update && apt-get -y install \
    openjdk-8-jre-headless \
    software-properties-common
RUN add-apt-repository -y ppa:ethereum/ethereum
RUN apt-get update && apt-get -y install \
    solc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /
COPY priv priv
COPY application.properties .
COPY helen*.jar .

# prepare for docker-compose, where athena is available from a virtual
# host named "athena1"
RUN sed -i -e "s/AthenaAuthorities=.*/AthenaAuthorities=athena1:5458,
athena2:5459,athena3:5460/g" application.properties
# and CockroachDB is available from a virtual host named db-server
RUN sed -i -e "s/DB_IP=.*/DB_IP=db-server/g" application.properties

CMD java -jar helen*.jar

EXPOSE 8080
