# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/agent/pom.xml ./persephone/agent/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/awsutil/pom.xml ./persephone/awsutil/pom.xml
COPY ./persephone/cli/pom.xml ./persephone/cli/pom.xml
COPY ./persephone/config-service/pom.xml ./persephone/config-service/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
COPY ./persephone/fleet-service/pom.xml ./persephone/fleet-service/pom.xml
COPY ./persephone/ip-allocation-service/pom.xml ./persephone/ip-allocation-service/pom.xml
COPY ./persephone/metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./persephone/orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./persephone/orchestration-vmc/pom.xml ./persephone/orchestration-vmc/pom.xml
COPY ./persephone/persistence/pom.xml ./persephone/persistence/pom.xml
COPY ./persephone/persistence-rocksdb/pom.xml ./persephone/persistence-rocksdb/pom.xml
COPY ./persephone/provisioning-service/pom.xml ./persephone/provisioning-service/pom.xml

## Run an empty package step to keep as much exogenous dependencies in a cached layer as possible.
RUN mvn -f persephone/pom.xml dependency:resolve-plugins dependency:resolve clean package -B

## Copy the dependent module sources.
COPY ./persephone/api ./persephone/api
COPY ./persephone/awsutil ./persephone/awsutil
COPY ./persephone/core ./persephone/core
COPY ./persephone/orchestration ./persephone/orchestration
COPY ./persephone/orchestration-vmc ./persephone/orchestration-vmc
COPY ./persephone/persistence ./persephone/persistence

## Copy the module sources.
COPY ./persephone/provisioning-service ./persephone/provisioning-service

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml package -pl provisioning-service -am -B

# Environment preparation.
FROM openjdk:11.0.2-jdk-slim as environment

RUN apt-get update \
    && apt-get install -y wget \
    && wget --directory-prefix=/tmp/ https://dl.cacerts.digicert.com/DigiCertSHA2HighAssuranceServerCA.crt \
    && keytool -importcert -alias vmwblockchain-cacert -cacerts -storepass changeit -file /tmp/DigiCertSHA2HighAssuranceServerCA.crt \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/test-configservice/configservice.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias vmwb-config-cert -file /tmp/configservice.crt

# Runtime image (Provisioning Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

## Ensure configtool can find it
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

## Disable TLS 1.3 due to buggy JDK implementation chewing up CPU.
RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

## Install CA certificate.
COPY --from=environment /docker-java-home/lib/security/cacerts /docker-java-home/lib/security/cacerts

WORKDIR /persephone/provisioning-service
COPY --from=builder ./persephone/provisioning-service/target/persephone-provisioning-service*with-dependencies.jar ./provisioning-service.jar

EXPOSE 9002

VOLUME ["/config"]

CMD ["java", "-jar", "provisioning-service.jar"]
