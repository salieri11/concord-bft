# This Dockerfile does not build Ethrpc, it just dockerizes it.
# You must build the UI and the HTTP Server first (see Readme.md).

## Run image
FROM ubuntu:16.04
LABEL Description="Ethrpc"

RUN apt-get update && apt-get -y install \
    openjdk-8-jre-headless \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /
COPY src/main/resources/application.properties .
COPY target/concord-ethrpc*.jar .

CMD java -jar concord-ethrpc*.jar

# Must match server.port in application.properties!
EXPOSE 8545
