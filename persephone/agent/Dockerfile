# Build and dockerize the concord-agent artifact.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ../resources/checkstyle.xml

## Copy the POM files (rarely changing).
COPY ./pom.xml ./pom.xml
COPY ./agent/pom.xml ./agent/pom.xml

## Build persephone API.
COPY ./api/src ./api/src

## Resolve / fetch all dependencies for agent module.
RUN mvn -f agent/pom.xml dependency:go-offline -B

## Build agent module.
COPY ./agent/src ./agent/src
RUN mvn -f agent/pom.xml install

# Environment preparation.
FROM openjdk:11.0.2-jdk-slim as environment

RUN apt-get update \
    && apt-get install --no-install-recommends -y wget

RUN wget --directory-prefix=/tmp/ https://github.com/theupdateframework/notary/releases/download/v0.6.1/notary-Linux-amd64

# Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.4
LABEL description="Node Agent"

## Get the notary binary from environment stage
## Make the binary executable
COPY --from=environment /tmp/notary-Linux-amd64 /usr/bin/notary-Linux-amd64
RUN chmod +x /usr/bin/notary-Linux-amd64

WORKDIR /agent

COPY ./agent/src/main/resources .
COPY --from=builder ./agent/target/node-agent*.jar ./node-agent.jar

VOLUME ["/config"]

CMD ["java", "-jar", "node-agent.jar"]
