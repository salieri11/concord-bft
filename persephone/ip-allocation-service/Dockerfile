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
COPY ./api ./persephone/api

## Copy the module sources.
RUN mvn -f pom.xml package -pl ip-allocation-service -am -B
#COPY ./ip-allocation-service ./persephone/ip-allocation-service

## Build module with all of its dependencies.
#RUN mvn -f ip-allocation-service/pom.xml install

# Runtime image (IP Allocation Service).
FROM athena-docker-local.artifactory.eng.vmware.com/vmbc-photon-3:1.0.4
LABEL description="VMware Blockchain Fleet Management IP Allocation Service"

WORKDIR /persephone/ip-allocation-service
COPY --from=builder ./ip-allocation-service/target/ip-allocation-service*.jar ./ip-allocation-service.jar
COPY ./resources/logging/log4j2.properties .

VOLUME ["/config"]

EXPOSE 9099

CMD ["java", "-jar", "ip-allocation-service.jar"]
