# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./pom.xml ./persephone/pom.xml
COPY ./agent/pom.xml ./persephone/agent/pom.xml
COPY ./api/pom.xml ./persephone/api/pom.xml
COPY ./cli/pom.xml ./persephone/cli/pom.xml
COPY ./config-service/pom.xml ./persephone/config-service/pom.xml
COPY ./core/pom.xml ./persephone/core/pom.xml
COPY ./fleet-service/pom.xml ./persephone/fleet-service/pom.xml
COPY ./ip-allocation-service/pom.xml ./persephone/ip-allocation-service/pom.xml
COPY ./metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./orchestration-vmware/pom.xml ./persephone/orchestration-vmware/pom.xml
COPY ./persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./persistence-rocksdb/pom.xml ./persephone/persistence-rocksdb/pom.xml
COPY ./provisioning-service/pom.xml ./persephone/provisioning-service/pom.xml

## Run an empty package step to keep as much exogenous dependencies in a cached layer as possible.
RUN mvn -f persephone/pom.xml dependency:resolve-plugins dependency:resolve clean package -B

## Copy the dependent module sources.
COPY ./api ./persephone/api
COPY ./core ./persephone/core
COPY ./persistence ./persephone/persistence

## Copy the module sources.
COPY ./metadata-service ./persephone/metadata-service

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml package -pl metadata-service -am -B

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/helen:prereqs-v2
LABEL description="VMware Blockchain Fleet Management Metadata Service"

WORKDIR /persephone/metadata-service
COPY --from=0 ./persephone/metadata-service/target/persephone-metadata-service*with-dependencies.jar ./metadata-service.jar

CMD ["java", "-jar", "metadata-service.jar"]

EXPOSE 9001
