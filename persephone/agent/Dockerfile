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
    && apt-get install -y wget \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/cacerts/config-service-test.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias vmwb-test-config-cert -file /tmp/config-service-test.crt \
    && wget --directory-prefix=/tmp/ http://aia.entrust.net/l1k-chain256.cer \
    && keytool -importcert -alias vmwb-prod-config-cert  -cacerts -storepass changeit -file /tmp/l1k-chain256.cer

# Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.0
LABEL description="Node Agent"

## Install CA certificate.
## This has hardcoded dependency on JAVA folder.
COPY --from=environment /docker-java-home/lib/security/cacerts /usr/local/zulu11.39.16-sa-jdk11.0.7-linux_x64/lib/security/cacerts

WORKDIR /agent

COPY ./agent/src/main/resources .
COPY --from=builder ./agent/target/node-agent*.jar ./node-agent.jar

VOLUME ["/config"]

CMD ["java", "-jar", "node-agent.jar"]
