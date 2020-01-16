# Copyright 2019 VMware, all rights reserved

FROM ubuntu:18.04 as builder
LABEL Description="Builder for the Thin Repica Client"

# autoconf <- log4cpp, grpc
# libtool <- grpc
RUN apt-get update && apt-get -y install \
    autoconf \
    build-essential \
    clang-7 \
    clang-format-7 \
    cmake \
    git \
    libtool \
    && rm -rf /var/lib/apt/lists/*

RUN ln -s /usr/bin/clang-format-7 /usr/bin/clang-format
RUN ln -s /usr/bin/clang-format-diff-7 /usr/bin/clang-format-diff

WORKDIR /
RUN git clone https://github.com/log4cplus/log4cplus.git && \
    cd /log4cplus && \
    git checkout REL_1_2_1 && \
    autoconf && \
    ./configure CXXFLAGS="--std=c++11 -march=x86-64 -mtune=generic" \
                --enable-static
# Combining the RUN above with the RUN below causes the log4cplus
# build to complain about missing aclocal-1.14.
WORKDIR /log4cplus
RUN make -j4 && \
    make install && \
    cd / && \
    rm -r log4cplus

# gRPC
WORKDIR /
RUN git clone https://github.com/grpc/grpc && \
    cd /grpc && \
    git checkout v1.17.x && \
    git submodule update --init && \
    cd /grpc/third_party/protobuf && \
    git checkout 3.6.x && \
    cd /grpc/third_party/protobuf && \
    ./autogen.sh && \
    ./configure --prefix=/opt/protobuf && \
    make -j4 && \
    make install && \
    cd /grpc && \
    make -j4 PROTOC=/opt/protobuf/bin/protoc && \
    make prefix=/opt/grpc install && \
    cd / && \
    rm -r grpc

# Copy sources
COPY ./concord/cmake /concord/cmake
COPY ./communication/src/main/proto /proto
COPY ./thin-replica-client /thin-replica-client

# Build
WORKDIR /thin-replica-client/build
RUN cmake -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
     -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7 \
     -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && \
     make -j$(nproc) all install

# TODO: Create runtime container image
ENV LOG4CPLUS_CONFIGURATION /thin-replica-client/example/log4cplus.properties
CMD ["/thin-replica-client/build/example/trc_example", "4", "1", \
     "concord1:50051", "concord2:50051", "concord3:50051", "concord4:50051"]
