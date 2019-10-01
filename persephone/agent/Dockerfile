# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./pom.xml ./persephone/pom.xml
COPY ./agent/pom.xml ./persephone/agent/pom.xml
COPY ./api/pom.xml ./persephone/api/pom.xml
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
COPY ./server-support/pom.xml ./persephone/server-support/pom.xml

## Run an empty package step to keep as much exogenous dependencies in a cached layer as possible.
RUN mvn -f persephone/pom.xml dependency:resolve-plugins dependency:resolve clean package -B

## Copy the dependent module sources.
COPY ./api ./persephone/api
COPY ./core ./persephone/core
COPY ./server-support ./persephone/server-support

## Copy the module sources.
COPY ./agent ./persephone/agent

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml package -pl agent -am -B

# Runtime image (Persephone Agent).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Concord Node Agent"

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/agent
COPY --from=0 ./persephone/agent/target/persephone-agent*with-dependencies.jar ./persephone-agent.jar

VOLUME ["/config"]

CMD ["java", "-jar", "persephone-agent.jar", "-Dkotlinx.coroutines.debug=on"]
