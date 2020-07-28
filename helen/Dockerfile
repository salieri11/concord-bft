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
COPY ./persephone/ip-allocation-service/pom.xml ./persephone/ip-allocation-service/pom.xml
COPY ./persephone/provisioning-service/pom.xml ./persephone/provisioning-service/pom.xml
COPY ./vmbc-base/pom.xml ./vmbc-base/pom.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./helen/pom.xml ./helen/pom.xml

#Install provisioning service dev certs in local trustore
USER root
RUN sed -i -e 's/archive.ubuntu.com/old-releases.ubuntu.com/g' /etc/apt/sources.list \
    && sed -i -e 's/security.ubuntu.com/old-releases.ubuntu.com/g' /etc/apt/sources.list \
    && apt-get update \
    && apt-get install -y wget \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/cacerts/ps-dev.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias ps-dev-cert -file /tmp/ps-dev.crt

# Switch to a user different than root.
RUN chown -R test:test .
USER test

# Resolve / fetch all dependencies for communication modules.
RUN mvn -f communication/pom.xml dependency:go-offline -B

# Build communication module.
COPY ./communication/src ./communication/src
RUN mvn -f communication/pom.xml install

# Build vmbc module.
RUN mvn -f vmbc-base/pom.xml dependency:go-offline -B
COPY ./vmbc-base/src ./vmbc-base/src
RUN mvn -f vmbc-base/pom.xml install

# Copy Persephone API files.
COPY ./persephone/api/src ./persephone/api/src

# Resolve / fetch all dependencies for helen module.
RUN mvn -f helen/pom.xml dependency:go-offline -B

# Build helen module.
COPY ./helen/src ./helen/src
RUN mvn -f helen/pom.xml package

## Runtime image.
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.1
LABEL description="Blockchain Management Service (Helen)"

WORKDIR /helen
COPY ./helen/src/main/resources/application.properties ./application.properties
COPY ./helen/src/main/resources/database/schema.sql ./db-schema.sql
COPY ./helen/wait-for-it.sh ./wait-for-it.sh
COPY --from=0 /home/test/helen/target/blockchain-helen*.jar ./blockchain-helen.jar

CMD ["java", "-Dspring.config.location=/config/app/profiles/,./", "-Dspring.profiles.active=${APPCONTEXT}", "-jar", "blockchain-helen.jar"]

EXPOSE 8080
