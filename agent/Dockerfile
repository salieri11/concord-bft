# Build and dockerize the concord-agent artifact.
FROM maven:3.6.1-jdk-11

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/cli/pom.xml ./persephone/cli/pom.xml
COPY ./persephone/awsutil/pom.xml ./persephone/awsutil/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
COPY ./persephone/metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./persephone/metadata-service-client/pom.xml ./persephone/metadata-service-client/pom.xml
COPY ./persephone/orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./persephone/orchestration-vmc/pom.xml ./persephone/orchestration-vmc/pom.xml
COPY ./persephone/persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./persephone/provision-service/pom.xml ./persephone/provision-service/pom.xml
COPY ./protobuf/pom.xml ./protobuf/pom.xml
COPY ./protobuf/grpc-runtime-kotlinx-serialization/pom.xml ./protobuf/grpc-runtime-kotlinx-serialization/pom.xml
COPY ./protobuf/protoc-plugin-grpc-kotlinx-serialization/pom.xml ./protobuf/protoc-plugin-grpc-kotlinx-serialization/pom.xml
COPY ./protobuf/protoc-plugin-kotlinx-serialization/pom.xml ./protobuf/protoc-plugin-kotlinx-serialization/pom.xml
COPY ./protobuf/protoc-plugin-kotlinx-serialization-test/pom.xml ./protobuf/protoc-plugin-kotlinx-serialization-test/pom.xml

COPY ./agent/pom.xml ./agent/pom.xml

# Build protobuf module.
COPY ./protobuf/grpc-runtime-kotlinx-serialization/src ./protobuf/grpc-runtime-kotlinx-serialization/src
COPY ./protobuf/protoc-plugin-grpc-kotlinx-serialization/src ./protobuf/protoc-plugin-grpc-kotlinx-serialization/src
COPY ./protobuf/protoc-plugin-kotlinx-serialization/src ./protobuf/protoc-plugin-kotlinx-serialization/src
RUN mvn -f protobuf/pom.xml install

# Build persephone API.
COPY ./persephone/api/src ./persephone/api/src
RUN mvn -f persephone/pom.xml install -pl api -am -B

# Build persephone AWS Util.
COPY ./persephone/awsutil/src ./persephone/awsutil/src
COPY ./persephone/awsutil/test ./persephone/awsutil/test
RUN mvn -f persephone/pom.xml install -pl awsutil -am -B

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
