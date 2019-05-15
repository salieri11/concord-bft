# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Build protobuf module.
COPY ./protobuf ./protobuf
RUN mvn -f protobuf/pom.xml install

## Copy the dependency-related POM files (rarely changing).
COPY ./helen/developer-config/checkstyle.xml ./helen/developer-config/checkstyle.xml
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

## Copy the dependency module sources and POM files.
COPY ./persephone/api/src ./persephone/api/src
COPY ./persephone/core/src ./persephone/core/src
COPY ./persephone/core/test ./persephone/core/test
COPY ./persephone/persistence/src ./persephone/persistence/src
COPY ./persephone/persistence/test ./persephone/persistence/test

## Copy the module sources and POM file.
COPY ./persephone/metadata-service/src ./persephone/metadata-service/src
COPY ./persephone/metadata-service/test ./persephone/metadata-service/test

## Build persephone modules.
RUN mvn -f persephone/pom.xml package -pl metadata-service -am -B

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="VMware Blockchain Fleet Management Metadata Service"

WORKDIR /persephone/metadata-service
COPY --from=0 ./persephone/metadata-service/target/persephone-metadata-service*with-dependencies.jar ./metadata-service.jar

CMD ["java", "-jar", "metadata-service.jar"]

EXPOSE 9001
