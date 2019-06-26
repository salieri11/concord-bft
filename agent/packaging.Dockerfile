# Dockerize locally pre-built Ethreaum RPC server artifact.
## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="RPC Server for Concord-Agent-Deployment service"

WORKDIR /agent
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/concord-agent*.jar ./concord-agent.jar

CMD ["java", "-jar", "concord-agent.jar", "config.json"]

# Must match server.port in application.properties!
EXPOSE 8645
