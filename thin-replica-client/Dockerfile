# Copyright 2019-2020 VMware, all rights reserved

# Dockerfile for building the Thin Replica Client Library image.

# Dependencies image.
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v16 \
      as dependencies

# Base runtime image.
FROM ubuntu:18.04
LABEL Description="Thin Replica Client Library"

# Install packaged runtime dependencies.
RUN apt-get update && apt-get -y install \
    build-essential \
    clang-7 \
    clang-format-7 \
    cmake \
    zlib1g-dev \
    libcurl4-openssl-dev \
    libboost-atomic1.65.1 \
    libboost-chrono1.65.1 \
    libboost-date-time1.65.1 \
    libboost-filesystem1.65.1 \
    libboost-program-options1.65.1 \
    libboost-system1.65.1 \
    libboost-thread1.65.1 \
    libyaml-cpp0.5v5 \
    libyaml-cpp-dev \
    && rm -rf /var/lib/apt/lists/*

RUN ln -s /usr/bin/clang-format-7 /usr/bin/clang-format
RUN ln -s /usr/bin/clang-format-diff-7 /usr/bin/clang-format-diff

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/opt/grpc/lib:/opt/protobuf/lib

# Copy dependencies from the prereqs image.
# Note symbolic links are manually created or recreated as just copying them
# causes the build to actually make a copy of their target, which increases the
# size of the image if we already have a copy of the target elsewhere.

# log4cplus
COPY --from=0 /usr/local/include/log4cplus /usr/local/include/log4cplus
COPY --from=0 /usr/local/lib/liblog4cplus-1.2.so.5.1.6 /usr/local/lib/
RUN ln -s /usr/local/lib/liblog4cplus-1.2.so.5.1.6 /usr/local/lib/liblog4cplus-1.2.so.5
RUN ln -s /usr/local/lib/liblog4cplus-1.2.so.5.1.6 /usr/local/lib/liblog4cplus.so

# OpenSSL
COPY --from=0 /usr/local/include/openssl /usr/local/include/openssl
COPY --from=0 /usr/local/lib/libcrypto.so.1.1 /usr/local/lib/
RUN ln -s /usr/local/lib/libcrypto.so.1.1 /usr/local/lib/libcrypto.so
COPY --from=0 /usr/local/lib/libssl.so.1.1 /usr/local/lib/
RUN ln -s /usr/local/lib/libssl.so.1.1 /usr/local/lib/libssl.so

# gRPC
COPY --from=0 /usr/include/google/protobuf /usr/include/google/protobuf
COPY --from=0 /opt/protobuf/bin /opt/protobuf/bin
COPY --from=0 /opt/protobuf/include /opt/protobuf/include
COPY --from=0 /opt/protobuf/lib/*.so.17.0.0 /opt/protobuf/lib/
RUN for x in /opt/protobuf/lib/*.so.17.0.0; do \
        ln -s $x ${x%%.0.0} && \
        ln -s $x ${x%%.17.0.0}; \
    done

COPY --from=0 /opt/grpc/bin /opt/grpc/bin
COPY --from=0 /opt/grpc/include /opt/grpc/include
COPY --from=0 /opt/grpc/lib/*.so.7.0.0 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.7.0.0; do \
        ln -s $x ${x%%.0.0} && \
        ln -s $x ${x%%.7.0.0}; \
    done
COPY --from=0 /opt/grpc/lib/*.so.1.17.1 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.1.17.1; do \
        ln -s $x ${x%%.17.1} && \
        ln -s $x ${x%%.1.17.1}; \
    done

# Thrift
COPY --from=0 /usr/local/include/thrift /usr/local/include/thrift
COPY --from=0 /usr/local/lib/libthrift-0.11.0.so /usr/local/lib
RUN ln -s /usr/local/lib/libthrift-0.11.0.so /usr/local/lib/libthrift.so

# OpenTracing
COPY --from=0 /usr/local/include/opentracing /usr/local/include/opentracing
COPY --from=0 /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so.1
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so

# Jaeger
COPY --from=0 /usr/local/include/jaegertracing /usr/local/include/jaegertracing
COPY --from=0 /usr/local/lib/libjaegertracing.a /usr/local/lib

# Googletest
COPY --from=0 /googletest /googletest

# Prometheus-cpp
COPY --from=0 /usr/local/lib/cmake/prometheus-cpp \
              /usr/local/lib/cmake/prometheus-cpp
COPY --from=0 /usr/local/include/prometheus /usr/local/include/prometheus

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

# Copy sources
COPY ./concord/cmake /concord/cmake
COPY ./communication/src/main/proto /communication/src/main/proto
COPY ./thin-replica-client /thin-replica-client

# Build Thin Replica Client Library
WORKDIR /thin-replica-client/build
RUN cmake -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
     -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7 \
     -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && \
     make -j$(nproc) all test && \
     make -j$(nproc) install
