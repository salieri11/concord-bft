# Dockerize locally pre-built agent server artifact.
## Runtime image.
#FROM athena-docker-local.artifactory.eng.vmware.com/agent:prereqs-v2
#LABEL description="Lifecycle-Agent"

WORKDIR /agent
COPY ./src/main/resources/application.properties ./application.properties
COPY ./target/concord-agent*.jar ./concord-agent.jar

CMD ["java", "-jar", "concord-agent.jar"]

# Must match server.port in application.properties!
EXPOSE 8546
