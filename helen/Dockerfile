# Helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.
FROM athena-docker-local.artifactory.eng.vmware.com/build-images/maven-builder:v1

# env variable for parameterizing application profile
ENV APPCONTEXT test

# config volume for exposing app profiles
VOLUME ["/config"]

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./communication/pom.xml ./communication/pom.xml
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml
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

## Copy the dependency-related POM files (rarely changing).
COPY ./helen/pom.xml ./helen/pom.xml

# Switch to a user different than root.
USER root
RUN chown -R test:test .
USER test

# Resolve / fetch all dependencies for communication module.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

# Copy Persephone API files.
COPY ./persephone/api/src ./persephone/api/src

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
COPY ./helen/wait-for-it.sh ./wait-for-it.sh
COPY --from=0 /home/test/helen/target/blockchain-helen*.jar ./blockchain-helen.jar

CMD ["java", "-Dspring.config.location=/config/app/profiles/,./", "-Dspring.profiles.active=${APPCONTEXT}", "-jar", "blockchain-helen.jar"]

EXPOSE 8080
