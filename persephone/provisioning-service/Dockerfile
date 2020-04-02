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