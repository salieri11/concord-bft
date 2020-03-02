# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./resources/checkstyle.xml ../resources/checkstyle.xml

## Copy the POM files (rarely changing).
COPY ./pom.xml ./pom.xml
COPY ./ip-allocation-service/pom.xml ./ip-allocation-service/pom.xml

## Copy the dependent module sources.
COPY ./api ./persephone/api

## Copy the module sources.
COPY ./ip-allocation-service ./persephone/ip-allocation-service

## Build module with all of its dependencies.
RUN mvn -f ip-allocation-service/pom.xml install

# Runtime image (IP Allocation Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management IP Allocation Service"

## Disable TLS 1.3 due to buggy JDK implementation chewing up CPU.
RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/ip-allocation-service
COPY --from=builder ./ip-allocation-service/target/persephone-ip-allocation-service*.jar ./ip-allocation-service.jar
COPY ./resources/logging/log4j2.properties .

VOLUME ["/config"]

EXPOSE 9099

CMD ["java", "-jar", "ip-allocation-service.jar"]
