# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./pom.xml ./persephone/pom.xml
COPY ./api/pom.xml ./persephone/api/pom.xml
COPY ./cli/pom.xml ./persephone/cli/pom.xml
COPY ./core/pom.xml ./persephone/core/pom.xml
COPY ./awsutil/pom.xml ./persephone/awsutil/pom.xml
COPY ./metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./metadata-service-client/pom.xml ./persephone/metadata-service-client/pom.xml
COPY ./orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./orchestration-vmc/pom.xml ./persephone/orchestration-vmc/pom.xml
COPY ./persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./provision-service/pom.xml ./persephone/provision-service/pom.xml

## Copy the dependency module sources and POM files.
COPY ./api/src ./persephone/api/src
COPY ./core/src ./persephone/core/src
COPY ./core/test ./persephone/core/test
COPY ./persistence/src ./persephone/persistence/src
COPY ./persistence/test ./persephone/persistence/test

## Copy the module sources and POM file.
COPY ./metadata-service/src ./persephone/metadata-service/src
COPY ./metadata-service/test ./persephone/metadata-service/test

## Build persephone modules.
RUN mvn -f persephone/pom.xml package -pl metadata-service -am -B

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="VMware Blockchain Fleet Management Metadata Service"

WORKDIR /persephone/metadata-service
COPY --from=0 ./persephone/metadata-service/target/persephone-metadata-service*with-dependencies.jar ./metadata-service.jar

CMD ["java", "-jar", "metadata-service.jar"]

EXPOSE 9001
