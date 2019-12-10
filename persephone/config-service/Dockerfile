# Arguments to select a Concord image.
# The Persephone configuration service needs to copy a binary for Concord's
# configuration generation utility from a Concord Docker image as part of its
# build process. This Concord image should generally be the one that the version
# of the configuration service being built targets deploying. By default, this
# Dockerfile targets using concord-core:latest, but a different Concord image
# can be targeted via build arguments. For example, to build
# persephone-configuration:512 to target deployment of Concord version 512 from
# Artifactory and use its conc_genconfig binary, one can run (from the root
# directory of this repo):
# ~/vmwathena_blockchain$ docker build -f persephone/config-service/Dockerfile \
#   -t persephone-configuration:512 --build-arg \
#   "concord_repo=athena-docker-local.artifactory.eng.vmware.com/concord-core" \
#   --build-arg "concord_tag=512"
ARG concord_repo="concord-core"
ARG concord_tag="latest"

# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
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

## Run an empty package step to keep as much exogenous dependencies in a cached layer as possible.
RUN mvn -f persephone/pom.xml dependency:resolve-plugins dependency:resolve clean package -B

## Copy the dependent module sources.
COPY ./persephone/api ./persephone/api
COPY ./persephone/core ./persephone/core
COPY ./persephone/server-support ./persephone/server-support

## Copy the module sources.
COPY ./persephone/config-service ./persephone/config-service

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml install -pl 'api' -am -B
RUN mvn -f persephone/pom.xml package -pl config-service -am -B

# Concord's configuration generation utility, conc_genconfig, is statically
# linked when Concord is built so that it can just be copied into the Persephone
# configuration service's image without needing to build Concord in Persephone's
# Dockerfile or copy its runtime dependencies into the configuration service
# image.
FROM ${concord_repo}:${concord_tag} as concord_image

# Runtime image (Metadata Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

WORKDIR /app
COPY --from=concord_image /concord/conc_genconfig /app/

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone-config-service
COPY --from=0 ./persephone/config-service/target/persephone-config-service*with-dependencies.jar ./config-service.jar

ENTRYPOINT ["java", "-jar", "config-service.jar"]

EXPOSE 9003

VOLUME ["/config"]
