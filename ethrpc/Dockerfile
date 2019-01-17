# Build and dockerize the Ethreaum RPC server artifact.
FROM maven:3.6.0-jdk-11

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./communication/pom.xml ./communication/pom.xml
COPY ./ethrpc/pom.xml ./ethrpc/pom.xml

# Resolve / fetch all dependencies for communication module.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

# Resolve / fetch all dependencies for ethrpc module.
RUN mvn -f ethrpc/pom.xml dependency:go-offline -B

# Build ethrpc module.
COPY ./ethrpc/src ./ethrpc/src
RUN mvn -f ethrpc/pom.xml package

## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="RPC Server for Ethereum API"

WORKDIR /ethrpc
COPY ./ethrpc/src/main/resources/application.properties ./application.properties
COPY --from=0 ./ethrpc/target/concord-ethrpc*.jar ./concord-ethrpc.jar

CMD ["java", "-jar", "concord-ethrpc.jar"]

# Must match server.port in application.properties!
EXPOSE 8545
