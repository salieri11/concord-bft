# Copyright 2019-2020 VMware, all rights reserved

FROM ubuntu:18.04 as builder
LABEL Description="Thin Replica Client Library"

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

# Googletest
WORKDIR /
RUN git clone https://github.com/google/googletest.git && \
    cd /googletest && \
    git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
WORKDIR /googletest/_build
RUN cmake -DCMAKE_CXX_FLAGS="-std=c++11 -march=x86-64 -mtune=generic" .. && make -j$(nproc)

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
