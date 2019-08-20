# Build and dockerize the concord-agent artifact.
FROM maven:3.6.1-jdk-11

## Copy build-dependent resource files.
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml

# Copy the POM files (rarely changing).
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/agent/pom.xml ./persephone/agent/pom.xml
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
COPY ./agent/pom.xml ./agent/pom.xml

# Build persephone API.
COPY ./persephone/api/src ./persephone/api/src
RUN mvn -f persephone/pom.xml install -pl api -am -B

# Resolve / fetch all dependencies for agent module.
RUN mvn -f agent/pom.xml dependency:go-offline -B

# Build agent module.
COPY ./agent/src ./agent/src
RUN mvn -f agent/pom.xml install

## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="Concord-Agent API"

WORKDIR /agent
COPY ./agent/src/main/resources/application.properties ./application.properties
COPY ./agent/src/main/resources/log4j2.xml ./log4j2.xml
#COPY --from=0 ./agent/target/concord-agent*with-dependencies.jar ./concord-agent.jar
COPY --from=0 ./agent/target/concord-agent-1.0-SNAPSHOT.jar ./concord-agent.jar

VOLUME ["/config"]

CMD ["java", "-jar", "concord-agent.jar"]

# Must match server.port in application.properties!
EXPOSE 8546
