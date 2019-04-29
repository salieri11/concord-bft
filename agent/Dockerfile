# Build and dockerize the concord-agent artifact.
FROM maven:3.6.0-jdk-11

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./communication/pom.xml ./communication/pom.xml
COPY ./agent/pom.xml ./agent/pom.xml

# Resolve / fetch all dependencies for communication module.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

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

CMD ["java", "-jar", "concord-agent.jar"]

# Must match server.port in application.properties!
EXPOSE 8546
