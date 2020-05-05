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
    zlib1g-dev \
    libcurl4-openssl-dev \
    wget \
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

WORKDIR /
RUN git clone -b REL_1_2_1 --depth 1 https://github.com/log4cplus/log4cplus.git && \
   cd /log4cplus && \
   autoreconf && \
   ./configure CXXFLAGS="--std=c++11 -march=x86-64 -mtune=generic" --enable-static && \
   make -j$(nproc) && \
   make install && \
   cd / && \
   rm -r log4cplus

WORKDIR /
RUN git clone https://svc.blockhain_1:Nn2NPdU6Ny9ZaYyXs3DB@gitlab.eng.vmware.com/core-build/openssl.git && \
    cd /openssl && \
    git checkout OpenSSL_1_1_1a && \
    ./config && \
    make -j4 && \
    make test && \
    make install && \
    cd / && \
    rm -r openssl

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

# thrift is a jaeger dependency
WORKDIR /
RUN wget http://apache.mirrors.hoobly.com/thrift/0.11.0/thrift-0.11.0.tar.gz && \
    tar xzf thrift-0.11.0.tar.gz && \
    cd thrift-0.11.0 && \
    ./configure && \
    make -j4 && \
    make install && \
    cd .. && \
    rm -r thrift-0.11.0

# opentracing is a jaeger dependency
WORKDIR /
RUN git clone https://github.com/opentracing/opentracing-cpp && \
    cd opentracing-cpp && \
    git checkout v1.5.0 && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j4 && \
    make install && \
    cd ../.. && \
    rm -r opentracing-cpp

# json is a jaeger dependency
WORKDIR /
RUN git clone https://github.com/nlohmann/json && \
    cd json && \
    git checkout v3.7.3 && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j4 && \
    make install && \
    cd ../.. && \
    rm -r json

# Jaeger build notes:
#
# * Jaeger wants to use the Hunter dependency manager, but this
#   ignores the dependencies (like Boost) we have already
#   installed. So, this build the deps manually for now.
#
# * By the same token, trc-lib needs a Findthrift.cmake, so we copy
#   Jaeger's to the common cmake location.
WORKDIR /
RUN git clone https://github.com/jaegertracing/jaeger-client-cpp && \
    cd jaeger-client-cpp && \
    git checkout v0.5.0 && \
    mkdir build && \
    cd build && \
    cmake -DHUNTER_ENABLED=NO -DBUILD_TESTING=NO -DBUILD_SHARED_LIBS=NO -DJAEGERTRACING_BUILD_EXAMPLES=NO .. && \
    make -j4 && \
    make install && \
    cp ../cmake/Findthrift.cmake /usr/share/cmake-3.10/Modules/ && \
    cd ../.. && \
    rm -r jaeger-client-cpp

# Jaeger really wants to find BoostConfig.cmake, not
# FindBoost.cmake. But, this wasn't introduced until boost 1.70. This
# doesn't matter anyway, because the jaegertracing.cmake finds
# FindBoost.cmake first anyway. The following sed just removes the
# search for BoostConfig.cmake.
RUN sed -i '/boost_components/d' /usr/local/lib/cmake/jaegertracing/jaegertracingConfig.cmake

ENV LD_LIBRARY_PATH /usr/local/lib

# Googletest
WORKDIR /
RUN git clone https://github.com/google/googletest.git && \
    cd /googletest && \
    git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
WORKDIR /googletest/_build
RUN cmake -DCMAKE_CXX_FLAGS="-std=c++11 -march=x86-64 -mtune=generic" .. && make -j$(nproc)

# Install Prometheus-cpp
WORKDIR /
RUN git clone https://github.com/jupp0r/prometheus-cpp.git && \
    cd prometheus-cpp && \
    git checkout v0.8.0 && \
    git submodule init && \
    git submodule update && \
    mkdir _build && \
    cd _build && \
    cmake -DBUILD_SHARED_LIBS=ON .. && \
    make && \
    make install && \
    cd ../.. && \
    rm -r prometheus-cpp

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
