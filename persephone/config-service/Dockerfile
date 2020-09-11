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
COPY ./persephone/resources/checkstyle.xml ../resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/config-service/pom.xml ./persephone/config-service/pom.xml

## Copy the dependent module sources.
COPY ./persephone/api ./persephone/api

## Copy the module sources.
COPY ./persephone/config-service ./persephone/config-service

## Build agent module.
RUN mvn -f ./persephone/config-service/pom.xml install

# Concord's configuration generation utility, conc_genconfig, is statically
# linked when Concord is built so that it can just be copied into the Persephone
# configuration service's image without needing to build Concord in Persephone's
# Dockerfile or copy its runtime dependencies into the configuration service
# image.

FROM ${concord_repo}:${concord_tag} as concord_image
#FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:0.6.0.968 as concord_image

# Runtime image (Metadata Service).
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.2
LABEL description="VMware Blockchain Configuration Service"

WORKDIR /app
COPY --from=concord_image /concord/conc_genconfig /app/
COPY --from=concord_image /usr/local/lib/liblog4cplus-2.0.so.3 /usr/local/lib/

WORKDIR /persephone-config-service
COPY ./persephone/config-service/src/main/resources .
COPY ./persephone/resources/logging/log4j2.properties .

COPY --from=0 ./persephone/config-service/target/persephone-config-service*.jar ./config-service.jar

# env variable for parameterizing application profile
ENV APPCONTEXT test

ENTRYPOINT ["java", "-Dspring.config.location=/config/app/profiles/,./", "-Dspring.profiles.active=${APPCONTEXT}",  "-jar", "config-service.jar"]

EXPOSE 9003
EXPOSE 9023

VOLUME ["/config"]
