# Helen needs cockroach DB to run, when running helen inside a
# container, we should also start another container for cockroach DB.
# The `run_cockroachdb_docker.sh` script starts a cockroachDB container
# and configures it for helen. Run that script first before starting helen
# containers.
FROM maven:3.6.0-jdk-11

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml
COPY ./communication/pom.xml ./communication/pom.xml
COPY ./helen/pom.xml ./helen/pom.xml

# Resolve / fetch all dependencies for communication module.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

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
COPY --from=0 ./helen/target/blockchain-helen*.jar ./blockchain-helen.jar

# prepare for docker-compose, where concord is available from a virtual
# hosts named "concord1", "concord2", ...
RUN echo "ConcordAuthorities=concord1:5458,concord2:5458,concord3:5458,concord4:5458" > ./application-test.properties
# and CockroachDB is available from a virtual host named db-server
RUN echo "DB_IP=db-server" >> ./application-test.properties

CMD ["java", "-Dspring.profiles.active=test", "-jar", "blockchain-helen.jar"]

EXPOSE 8080
