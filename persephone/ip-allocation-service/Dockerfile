# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./pom.xml ./persephone/pom.xml
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
COPY ./persistence ./persephone/persistence
COPY ./persistence-rocksdb ./persephone/persistence-rocksdb
COPY ./server-support ./persephone/server-support

## Copy the module sources.
COPY ./ip-allocation-service ./persephone/ip-allocation-service

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml package -pl ip-allocation-service -am -B

# Runtime image (IP Allocation Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management IP Allocation Service"

## Disable TLS 1.3 due to buggy JDK implementation chewing up CPU.
RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/ip-allocation-service
COPY --from=builder ./persephone/ip-allocation-service/target/persephone-ip-allocation-service*with-dependencies.jar ./ip-allocation-service.jar

VOLUME ["/config"]

EXPOSE 9099

CMD ["java", "-jar", "ip-allocation-service.jar", "-Dkotlinx.coroutines.debug=on"]
