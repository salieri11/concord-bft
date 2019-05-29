# Build time image.
FROM maven:3.6.1-jdk-11 as builder

## Copy build-dependent resource files.
COPY ./persephone/resources/checkstyle.xml ./persephone/resources/checkstyle.xml

## Copy the dependency-related POM files (rarely changing).
COPY ./persephone/pom.xml ./persephone/pom.xml
COPY ./persephone/api/pom.xml ./persephone/api/pom.xml
COPY ./persephone/awsutil/pom.xml ./persephone/awsutil/pom.xml
COPY ./persephone/cli/pom.xml ./persephone/cli/pom.xml
COPY ./persephone/core/pom.xml ./persephone/core/pom.xml
COPY ./persephone/metadata-service/pom.xml ./persephone/metadata-service/pom.xml
COPY ./persephone/metadata-service-client/pom.xml ./persephone/metadata-service-client/pom.xml
COPY ./persephone/orchestration/pom.xml ./persephone/orchestration/pom.xml
COPY ./persephone/orchestration-vmc/pom.xml ./persephone/orchestration-vmc/pom.xml
COPY ./persephone/persistence/pom.xml ./persephone/persistence/pom.xml
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

## Get the C++ part
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v8 as builder1

COPY ./communication /communication
COPY .git /.git

WORKDIR /concord
COPY ./concord /concord

WORKDIR /concord/build
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && make -j$(nproc) conc_genconfig

# Runtime image (Metadata Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

## Install libs build from source
COPY --from=builder1 /usr/local/lib/liblog4cplus* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libboost_program_options.so.* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libgmp* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libyaml* /usr/local/lib/

## Ensure configtool can find it
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

WORKDIR /app
COPY --from=builder1 /concord/build/tools/conc_genconfig /app/

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/provisioning-service
COPY --from=0 ./persephone/provisioning-service/target/persephone-provisioning-service*with-dependencies.jar ./provisioning-service.jar

EXPOSE 9002

VOLUME ["/config"]

CMD ["java", "-jar", "provisioning-service.jar"]
