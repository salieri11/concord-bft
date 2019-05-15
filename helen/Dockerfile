# Helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.
FROM athena-docker-local.artifactory.eng.vmware.com/build-images/maven-builder:v1

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./communication/pom.xml ./communication/pom.xml
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/cli/pom.xml ./persephone/cli/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
COPY ./persephone/awsutil/pom.xml ./persephone/awsutil/pom.xml
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

## Copy the dependency-related POM files (rarely changing).
COPY ./helen/pom.xml ./helen/pom.xml

# Switch to a user different than root.
USER root
RUN chown -R test:test .
USER test

# Build protobuf module.
COPY ./protobuf/grpc-runtime-kotlinx-serialization/src ./protobuf/grpc-runtime-kotlinx-serialization/src
COPY ./protobuf/protoc-plugin-grpc-kotlinx-serialization/src ./protobuf/protoc-plugin-grpc-kotlinx-serialization/src
COPY ./protobuf/protoc-plugin-kotlinx-serialization/src ./protobuf/protoc-plugin-kotlinx-serialization/src
RUN mvn -f protobuf/pom.xml install

# Resolve / fetch all dependencies for communication module.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

# Build persephone API.
COPY ./persephone/api/src ./persephone/api/src
RUN mvn -f persephone/pom.xml install -pl api -am -B

# Resolve / fetch all dependencies for helen module.
RUN mvn -f helen/pom.xml dependency:go-offline -B

# Build helen module.
COPY ./helen/src ./helen/src
RUN mvn -f helen/pom.xml package

## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="Blockchain Management Service (Helen)"

WORKDIR /helen
COPY ./helen/src/main/resources/application.properties ./application.properties
COPY ./helen/src/main/resources/database/schema.sql ./db-schema.sql
COPY --from=0 /home/test/helen/target/blockchain-helen*.jar ./blockchain-helen.jar

# prepare for docker-compose, where concord is available from a virtual
# hosts named "concord1", "concord2", ...
RUN echo "ConcordAuthorities=concord1:5458,concord2:5458,concord3:5458,concord4:5458" > ./application-test.properties
# and CockroachDB is available from a virtual host named db-server
RUN echo "DB_IP=db-server" >> ./application-test.properties

CMD ["java", "-Dspring.profiles.active=test", "-jar", "blockchain-helen.jar"]

EXPOSE 8080
