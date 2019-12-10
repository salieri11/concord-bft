# Build and dockerize the concord-agent artifact.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml

## Copy the POM files (rarely changing).
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/config-service/pom.xml ./persephone/config-service/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
COPY ./persephone/fleet-service/pom.xml ./persephone/fleet-service/pom.xml
COPY ./persephone/ip-allocation-service/pom.xml ./persephone/ip-allocation-service/pom.xml
COPY ./persephone/metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./persephone/orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./persephone/orchestration-vmware/pom.xml ./persephone/orchestration-vmware/pom.xml
COPY ./persephone/persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./persephone/persistence-rocksdb/pom.xml ./persephone/persistence-rocksdb/pom.xml
COPY ./persephone/provisioning-service/pom.xml ./persephone/provisioning-service/pom.xml
COPY ./persephone/server-support/pom.xml ./persephone/server-support/pom.xml
COPY ./agent/pom.xml ./agent/pom.xml

## Build persephone API.
COPY ./persephone/api/src ./persephone/api/src
RUN mvn -f persephone/pom.xml install -pl api -am -B

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
FROM openjdk:11.0.2-jdk-slim
LABEL description="Concord Agent"

## Install CA certificate.
COPY --from=environment /docker-java-home/lib/security/cacerts /docker-java-home/lib/security/cacerts

WORKDIR /agent
COPY ./agent/src/main/resources/application.properties ./application.properties
COPY ./agent/src/main/resources/log4j2.xml ./log4j2.xml
COPY --from=builder ./agent/target/concord-agent*.jar ./concord-agent.jar

VOLUME ["/config"]

CMD ["java", "-jar", "concord-agent.jar", "config.json"]
