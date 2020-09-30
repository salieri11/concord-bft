## IMPORTANT: You must have initialized and updated the submodules
## before building the docker image! See README.md.

## ALSO IMPORTANT: You must pass the parent directory as the path for
## this build. We need to copy files from ../communication, and Docker
## will not allow copying files from outside the build scope. Use either:
##
## from this directory: docker build -f Dockerfile ..
##         from parent: docker build -f concord/Dockerfile .

## Build image
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v26

## Remember context is the parent of this directory
COPY ./communication /communication

## Copy the blockchain repo .git directory so submodules can run git commands
## While this is somewhat dirty it makes the relative path in
## concord/submodule/concord-bft/.git work. Alternatively, we could change all
## the paths and copy the entire blockchain dir to /blockchain. However, this
## works fine, is a one line change, and the docker image is destroyed after
## build anyway.
COPY .git /.git

WORKDIR /concord
COPY ./concord /concord
WORKDIR /concord/build
RUN cmake \
    -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
    -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7 \
    .. && \
    CTEST_OUTPUT_ON_FAILURE=true make \
    -j$(nproc) all test

RUN ./tools/conc_genconfig --configuration-type application --output-name app --configuration-input ../resources/appconf_input.yaml
## Base Run image
FROM ubuntu:18.04
LABEL Description="Concord"

RUN apt-get update \
    && apt-get -y install bind9-host \
    gosu \
    dnsutils \
    libboost-atomic1.65.1 \
    libboost-chrono1.65.1 \
    libboost-date-time1.65.1 \
    libboost-filesystem1.65.1 \
    libboost-program-options1.65.1 \
    libboost-system1.65.1 \
    libboost-thread1.65.1 \
    libbz2-1.0 \
    libgmp10 \
    liblz4-1 \
    libs3-dev \
    libsnappy1v5 \
    libzstd1-dev \
    libyaml-cpp0.5v5 \
    llvm-5.0 \
    gdb \
    lldb \
    vim \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# SYMLINK NOTE: If you COPY a symlink, docker will copy the contents
# of the target file to a new file, doubling the space used in the
# image.

COPY --from=0 /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/
RUN ln -s /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/liblog4cplus-2.0.so.3
RUN ln -s /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/liblog4cplus.so

COPY --from=0 /usr/local/lib/libprometheus-cpp-core.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-core.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

COPY --from=0 /usr/local/lib/libprometheus-cpp-pull.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-pull.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

COPY --from=0 /usr/local/lib/libprometheus-cpp-push.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-push.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

# gRPC
COPY --from=0 /opt/protobuf/include /opt/protobuf/include
COPY --from=0 /opt/grpc/include /opt/grpc/include
COPY --from=0 /opt/grpc/bin /opt/grpc/bin

# Copy libraries and create symlinks (docker copies instead of sym linking)
COPY --from=0 /opt/protobuf/lib/*.so.3.13.0.0 /opt/protobuf/lib/
RUN for x in /opt/protobuf/lib/*.so.3.13.0.0; do \
        ln -s $x ${x%%.3.13.0.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.1.32.0 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.1.32.0; do \
        ln -s $x ${x%%.32.0} && \
        ln -s $x ${x%%.1.32.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.12.0.0 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.12.0.0; do \
        ln -s $x ${x%%.0.0} && \
        ln -s $x ${x%%.12.0.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.1.2.11 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.1.2.11; do \
        ln -s $x ${x%%.2.11} && \
        ln -s $x ${x%%.1.2.11}; \
    done

COPY --from=0 /opt/grpc/lib/libre2.so /opt/grpc/lib/

COPY --from=0 /opt/grpc/lib/libabsl_* /opt/grpc/lib/

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/opt/grpc/lib:/opt/protobuf/lib

# evmjit is statically compiled, so we don't need to copy

COPY --from=0 /usr/local/lib/librelic.so /usr/local/lib/

COPY --from=0 /usr/local/lib/librocksdb.so.6.8.1 /usr/local/lib/
RUN ln -s /usr/local/lib/librocksdb.so.6.8.1 /usr/local/lib/librocksdb.so.6.8
RUN ln -s /usr/local/lib/librocksdb.so.6.8.1 /usr/local/lib/librocksdb.so.6
RUN ln -s /usr/local/lib/librocksdb.so.6.8.1 /usr/local/lib/librocksdb.so

COPY --from=0 /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/
RUN ln -s /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/libsecp256k1.so.0
RUN ln -s /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/libsecp256k1.so

COPY --from=0 /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so.1
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so

COPY --from=0 /usr/local/lib/libthrift-0.11.0.so /usr/local/lib
RUN ln -s /usr/local/lib/libthrift-0.11.0.so /usr/local/lib/libthrift.so

COPY --from=0 /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib
RUN ln -s /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib/libhdr_histogram.so.6
RUN ln -s /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib/libhdr_histogram.so

WORKDIR /concord/resources
COPY --from=0 /concord/build/src/concord /concord/concord
COPY --from=0 /concord/build/tools/conc_* /concord/
COPY --from=0 /concord/build/tools/ec* /concord/
COPY --from=0 /concord/build/submodules/concord-bft/kvbc/tools/sparse_merkle_db/sparse_merkle_db_editor /concord/

COPY ./concord/test/concord.supp /concord/concord.supp

# Copy default Concord logger configuration so logging will be sanely configured
# if no custom configuration was provided when the cluster was set up.
COPY ./concord/resources/log4cplus.properties /concord/resources/log4cplus.properties
COPY ./concord/resources/metrics_config.yaml /concord/resources/metrics_config.yaml
COPY ./concord/resources/appconf_input.yaml /concord/resources/appconf_input.yaml
COPY ./concord/submodules/concord-bft/diagnostics/concord-ctl /concord

COPY --from=0 /concord/build/app1.config /concord/config/application.config

# Management port
EXPOSE 5458

# Ledger API service
EXPOSE 50051

# SBFT ports.
EXPOSE 3501/tcp
EXPOSE 3502/tcp
EXPOSE 3503/tcp
EXPOSE 3504/tcp
EXPOSE 3505/tcp

EXPOSE 3501/udp
EXPOSE 3502/udp
EXPOSE 3503/udp
EXPOSE 3504/udp
EXPOSE 3505/udp

# Mountable volumes.
VOLUME [ "/concord/config", "/concord/rocksdbdata", "/concord/log", "/concord/tls_certs", "/concord/trs_trc_tls_certs", "/concord/cores", "/concord/config-generated" ]

# Concord starting script
COPY ./concord/scripts/concord-entrypoint.sh /concord/concord-entrypoint.sh

## Add concord user
RUN groupadd -r -g 999 concord && useradd -r -g concord -u 999 concord

# Set working directory and start.
WORKDIR /concord
ENTRYPOINT ["/concord/concord-entrypoint.sh"]
