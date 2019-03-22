# Build time image.
FROM maven:3.6.0-jdk-11 as builder

## Build protobuf module.
COPY ./protobuf ./protobuf
RUN mvn -f protobuf/pom.xml install

## Copy the dependency-related POM files (rarely changing).
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/cli/pom.xml ./persephone/cli/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
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
COPY ./persephone/orchestration/src ./persephone/orchestration/src
COPY ./persephone/orchestration-vmc/src ./persephone/orchestration-vmc/src
COPY ./persephone/persistence/src ./persephone/persistence/src
COPY ./persephone/persistence/test ./persephone/persistence/test

## Copy the module sources and POM file.
COPY ./persephone/provision-service/src ./persephone/provision-service/src
COPY ./persephone/provision-service/test ./persephone/provision-service/test

## Build persephone modules.
RUN mvn -f persephone/pom.xml package -pl provision-service -am -B

# Runtime image (Metadata Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/provision-service
COPY --from=0 ./persephone/provision-service/target/persephone-provision-service*with-dependencies.jar ./provision-service.jar

EXPOSE 9002

VOLUME ["/config"]

CMD ["java", "-jar", "provision-service.jar"]
