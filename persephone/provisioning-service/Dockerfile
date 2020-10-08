# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ../resources/checkstyle.xml

## Copy the POM files (rarely changing).
COPY ./pom.xml ./pom.xml
COPY ./api/pom.xml ./api/pom.xml
COPY ./provisioning-service/pom.xml ./provisioning-service/pom.xml

COPY ./agent/pom.xml ./agent/pom.xml
COPY ./config-service/pom.xml ./config-service/pom.xml
COPY ./ip-allocation-service/pom.xml ./ip-allocation-service/pom.xml

## Copy the dependent module sources.
COPY ./api ./api

## Copy the module sources.
COPY ./provisioning-service  ./provisioning-service

## Build module with all of its dependencies.
RUN mvn -f pom.xml package -pl provisioning-service -am -B
#RUN mvn -f persephone/provisioning-service/pom.xml install

# Runtime image (Provisioning Service).
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.4
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

WORKDIR /persephone-provisioning-service
COPY ./provisioning-service/src/main/resources .
COPY ./resources/logging/log4j2.properties .

COPY --from=builder ./provisioning-service/target/persephone-provisioning-service*.jar ./provisioning-service.jar

# env variable for parameterizing application profile
ENV APPCONTEXT test

ENTRYPOINT ["java", "-Dspring.config.location=/config/app/profiles/,./", "-Dspring.profiles.active=${APPCONTEXT}",  "-jar", "provisioning-service.jar"]

# Expose port 8000 for debug or profiling usage.
# Expose service port
EXPOSE 8000 9002

VOLUME ["/config"]
VOLUME ["/certs"]
