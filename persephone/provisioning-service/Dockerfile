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
COPY ./orchestration ./persephone/orchestration
COPY ./orchestration-vmware ./persephone/orchestration-vmware
COPY ./persistence ./persephone/persistence
COPY ./server-support ./persephone/server-support

## Copy the module sources.
COPY ./provisioning-service ./persephone/provisioning-service

## Build module with all of its dependencies.
RUN mvn -f persephone/pom.xml package -pl provisioning-service -am -B

# Environment preparation.
FROM openjdk:11.0.2-jdk-slim as environment

RUN apt-get update \
    && apt-get install -y wget \
    && wget --directory-prefix=/tmp/ https://dl.cacerts.digicert.com/DigiCertSHA2HighAssuranceServerCA.crt \
    && keytool -importcert -alias vmwblockchain-cacert -cacerts -storepass changeit -file /tmp/DigiCertSHA2HighAssuranceServerCA.crt \
    && wget --directory-prefix=/tmp/ https://vmbc-saas.s3.us-east-2.amazonaws.com/cacerts/config-service-test.crt \
    && keytool -import -trustcacerts -cacerts -storepass changeit -noprompt -alias vmwb-test-config-cert -file /tmp/config-service-test.crt \
    && wget --directory-prefix=/tmp/ http://aia.entrust.net/l1k-chain256.cer \
    && keytool -importcert -alias vmwb-prod-config-cert  -cacerts -storepass changeit -file /tmp/l1k-chain256.cer

# Runtime image (Provisioning Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

## Disable TLS 1.3 due to buggy JDK implementation chewing up CPU.
RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

## Install CA certificate.
COPY --from=environment /docker-java-home/lib/security/cacerts /docker-java-home/lib/security/cacerts

WORKDIR /persephone/provisioning-service
COPY --from=builder ./persephone/provisioning-service/target/persephone-provisioning-service*with-dependencies.jar ./provisioning-service.jar

VOLUME ["/config"]

# Expose port 8000 for debug or profiling usage.
# Expose service port
EXPOSE 8000 9002

CMD ["java", "-jar", "provisioning-service.jar", "-Dkotlinx.coroutines.debug=on"]
