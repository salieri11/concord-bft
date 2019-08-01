# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="RPC Server for Concord-Agent-Deployment service"

# Environment preparation.
FROM openjdk:11.0.2-jdk-slim as environment

RUN apt-get update \
    && apt-get install -y wget \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/cacerts/config-service-test.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias vmwb-test-config-cert -file /tmp/config-service-test.crt \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/cacerts/config-service-prod.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias vmwb-prod-config-cert -file /tmp/config-service-prod.crt


FROM openjdk:11.0.2-jdk-slim

## Install CA certificate.
COPY --from=environment /docker-java-home/lib/security/cacerts /docker-java-home/lib/security/cacerts


WORKDIR /agent
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/concord-agent*.jar ./concord-agent.jar

CMD ["java", "-jar", "concord-agent.jar", "config.json"]

# Must match server.port in application.properties!
EXPOSE 8645
