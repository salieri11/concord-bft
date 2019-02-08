# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="RPC Server for Ethereum API"

WORKDIR /ethrpc
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/concord-ethrpc*.jar ./concord-ethrpc.jar

CMD ["java", "-jar", "concord-ethrpc.jar"]

# Must match server.port in application.properties!
EXPOSE 8545
