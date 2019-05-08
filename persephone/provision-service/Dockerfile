# Build time image.
FROM maven:3.6.1-jdk-11 as builder

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

## Get the C++ part
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v5 as builder1

COPY .git /.git

WORKDIR /concord
COPY ./concord /concord
WORKDIR /concord/build
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && make -j$(nproc) conc_genconfig

#FROM ubuntu:18.04
#LABEL Description="ConcordCfgTool"

# Install libs
#RUN apt-get update && apt-get -y install \
#    libgmp10 \
#    libboost-program-options1.65.1 \
#    libyaml-cpp0.5v5 \
#    && rm -rf /var/lib/apt/lists/*

# Install libs build from source
#COPY --from=builder1 /usr/local/lib/liblog4cplus* /usr/local/lib/

## Ensure configtool can find it
#ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

# Copy ConfigTool
#COPY --from=builder1 /concord/build/tools/conc_genconfig /app/



# Runtime image (Metadata Service).
FROM openjdk:11.0.2-jdk-slim
LABEL description="VMware Blockchain Fleet Management Provisioning Service"

# Install libs build from source
COPY --from=builder1 /usr/local/lib/liblog4cplus* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libboost_program_options.so.* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libgmp* /usr/local/lib/
COPY --from=builder1 /usr/lib/x86_64-linux-gnu/libyaml* /usr/local/lib/

# Ensure configtool can find it
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"

WORKDIR /app
COPY --from=builder1 /concord/build/tools/conc_genconfig /app/

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

WORKDIR /persephone/provision-service
COPY --from=0 ./persephone/provision-service/target/persephone-provision-service*with-dependencies.jar ./provision-service.jar


EXPOSE 9002

VOLUME ["/config"]

CMD ["java", "-jar", "provision-service.jar"]
