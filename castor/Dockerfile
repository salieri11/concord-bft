FROM athena-docker-local.artifactory.eng.vmware.com/build-images/maven-builder:v1

# env variable for parameterizing application profile

# config volume for exposing app profiles
VOLUME ["/config"]

# Copy the POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
COPY ./blockchain-build/pom.xml ./blockchain-build/pom.xml

COPY ./castor/pom.xml ./castor/pom.xml

# Switch to a user different than root.
USER root
RUN chown -R test:test .
USER test

# Copy Persephone API files (required for proto files)
COPY ./persephone/api/src ./persephone/api/src

# Resolve / fetch all dependencies for Castor module.
RUN mvn -f castor/pom.xml dependency:go-offline -B

# Build Castor module.
COPY ./castor/src ./castor/src
RUN mvn -f castor/pom.xml package

## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.4
LABEL description="VMware Blockchain Orchestrator"

COPY ./helen/wait-for-it.sh ./castor/wait-for-it.sh

# Copy on-prem deployment product (Orchestrator) runtime files. 
# This is mostly the docker-compose files required for Castor.
COPY ./castor/orchestrator-runtime ./orchestrator-runtime
# Also copy the json schemas so users can validate their instances
COPY ./castor/src/main/resources/infrastructure-descriptor-v1.schema ./orchestrator-runtime
COPY ./castor/src/main/resources/deployment-descriptor-v1.schema ./orchestrator-runtime

WORKDIR /castor
COPY ./castor/src/main/resources/application.properties ./application.properties
COPY --from=0 /home/test/castor/target/castor*.jar ./castor.jar

CMD ["java", "-Dspring.config.location=/config/app/profiles/,./", "-jar", "castor.jar"]

