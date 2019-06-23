# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./pom.xml ./persephone/pom.xml
COPY ./api/pom.xml ./persephone/api/pom.xml
COPY ./awsutil/pom.xml ./persephone/awsutil/pom.xml
COPY ./cli/pom.xml ./persephone/cli/pom.xml
COPY ./core/pom.xml ./persephone/core/pom.xml
COPY ./metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./metadata-service-client/pom.xml ./persephone/metadata-service-client/pom.xml
COPY ./orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./orchestration-vmc/pom.xml ./persephone/orchestration-vmc/pom.xml
COPY ./persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./persistence-rocksdb/pom.xml ./persephone/persistence-rocksdb/pom.xml
COPY ./provisioning-service/pom.xml ./persephone/provisioning-service/pom.xml

## Copy the dependency module sources and POM files.
COPY ./api ./persephone/api
COPY ./core ./persephone/core
COPY ./persistence ./persephone/persistence

## Copy the module sources and POM file.
COPY ./metadata-service-client/src ./persephone/metadata-service-client/src

## Build persephone modules.
RUN mvn -f persephone/pom.xml package -pl metadata-service-client -am -B

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="VMware Blockchain Fleet Management Metadata Service Client"

WORKDIR /persephone/metadata-service-client
COPY --from=0 ./persephone/metadata-service-client/target/persephone-metadata-service-client*with-dependencies.jar ./metadata-service-client.jar

ENTRYPOINT ["java", "-jar", "metadata-service-client.jar"]
