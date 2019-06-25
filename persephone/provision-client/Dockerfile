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
COPY ./provision-client/pom.xml ./persephone/provision-client/pom.xml

## Copy the dependency module sources and POM files.
COPY ./api ./persephone/api
COPY ./core ./persephone/core
COPY ./orchestration ./persephone/orchestration
COPY ./orchestration-vmc ./persephone/orchestration-vmc

## Copy the module sources and POM file.
COPY ./provision-client/src ./persephone/provision-client/src

## Build persephone modules.
RUN mvn -f persephone/pom.xml install -pl 'api,core,orchestration,orchestration-vmc' -am -B
RUN mvn -f persephone/provision-client/pom.xml package -B

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="VMware Blockchain Fleet Management Provisioning Service Client"

WORKDIR /provisioning-client
COPY --from=0 ./persephone/provision-client/target/persephone-provisioning-client*with-dependencies.jar ./provisioning-client.jar

ENTRYPOINT ["java", "-jar", "provisioning-client.jar"]
