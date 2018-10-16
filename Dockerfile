## IMPORTANT: You must have initialized and updated the submodules
## before building the docker image! See README.md.

## Build image
FROM ubuntu:16.04
LABEL Description="Build environment for Athena"

RUN apt-get update && apt-get -y install \
    autoconf \
    automake \
    cmake \
    g++ \
    git \
    libbz2-dev \
    libgmp3-dev \
    liblz4-dev \
    libprotobuf-dev \
    libsnappy-dev \
    libtool \
    libzstd-dev \
    llvm-5.0 \
    llvm-5.0-dev \
    protobuf-compiler \
    python2.7 \
    wget \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /
RUN git clone https://github.com/google/googletest.git
WORKDIR /googletest
RUN git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
WORKDIR /googletest/_build
RUN cmake -DCMAKE_CXX_FLAGS="-std=c++11 -march=x86-64 -mtune=generic" .. && make

WORKDIR /
RUN git clone https://github.com/weidai11/cryptopp.git
WORKDIR /cryptopp
RUN git checkout CRYPTOPP_5_6_5
COPY ./cross-platform-cryptopp.patch .
RUN git apply --whitespace=nowarn cross-platform-cryptopp.patch
WORKDIR /cryptopp/build
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && make && make install

WORKDIR /
RUN wget https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.tar.gz \
    && tar -xzf boost_1_64_0.tar.gz \
    && rm boost_1_64_0.tar.gz
WORKDIR /boost_1_64_0
RUN ./bootstrap.sh --with-libraries=system,program_options,thread --prefix=/usr && ./b2 cxxflags="-march=x86-64 -mtune=generic" && ./b2 install

WORKDIR /
RUN git clone https://github.com/log4cplus/log4cplus.git
WORKDIR /log4cplus
RUN git checkout REL_1_2_1
RUN sed -i -e "s/am__api_version='1.14'/am__api_version='1.15'/g" configure
RUN ./configure CXXFLAGS="--std=c++11 -march=x86-64 -mtune=generic" && make && make install

WORKDIR /
RUN git clone https://github.com/ethereum/evmjit.git
WORKDIR /evmjit
### their interface is evolving rapidly, so use this specific hash
RUN git checkout 4e9f3d76292c7de0c6613427761f843b1719f614
RUN mkdir build
WORKDIR /evmjit/build
RUN cmake -DLLVM_DIR=/usr/lib/llvm-5.0/lib/cmake/llvm -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" ..
RUN cmake --build . --config RelWithDebInfo

WORKDIR /
RUN git clone https://github.com/relic-toolkit/relic
WORKDIR /relic/build
RUN cmake -DALLOC=AUTO -DWSIZE=64 -DWORD=64 -DRAND=UDEV -DSHLIB=ON -DSTLIB=ON -DSTBIN=OFF -DTIMER=HREAL -DCHECK=on -DVERBS=on -DARITH=x64-asm-254 -DFP_PRIME=254 -DFP_METHD="INTEG;INTEG;INTEG;MONTY;LOWER;SLIDE" -DCOMP="-O3 -funroll-loops -fomit-frame-pointer -finline-small-functions -march=x86-64 -mtune=generic" -DFP_PMERS=off -DFP_QNRES=on -DFPX_METHD="INTEG;INTEG;LAZYR" -DPP_METHD="LAZYR;OATEP" .. && make && make install

WORKDIR /
RUN wget https://github.com/facebook/rocksdb/archive/v5.7.3.tar.gz \
    && tar -xzf v5.7.3.tar.gz \
    && rm v5.7.3.tar.gz
WORKDIR /rocksdb-5.7.3
RUN PORTABLE=1 make shared_lib && PORTABLE=1 make install-shared

WORKDIR /
RUN git clone https://github.com/bitcoin-core/secp256k1
WORKDIR /secp256k1
RUN git checkout 1e6f1f5ad5e7f1e3ef79313ec02023902bf8175c
RUN ./autogen.sh && ./configure --enable-module-recovery CFLAGS="-march=x86-64 -mtune=generic" CPPFLAGS="-march=x86-64 -mtune=generic"
RUN make && make install

WORKDIR /athena
COPY . /athena
WORKDIR /athena/build
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && make


## Base Run image
FROM ubuntu:16.04
LABEL Description="Athena"

RUN apt-get update && apt-get -y install \
    bind9-host \
    libbz2-1.0 \
    libgmp10 \
    liblz4-1 \
    libprotobuf9v5 \
    libsnappy1v5 \
    libzstd0 \
    llvm-5.0 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/local/lib/libcryptopp* /usr/local/lib/
COPY --from=0 /usr/lib/libboost* /usr/lib/
COPY --from=0 /usr/local/lib/liblog4cplus* /usr/local/lib/
# evmjit is statically compiled, so we don't need to copy
COPY --from=0 /usr/local/lib/librelic* /usr/local/lib/
COPY --from=0 /usr/local/lib/librocksdb.* /usr/local/lib/
COPY --from=0 /usr/local/lib/libsecp256k1* /usr/local/lib/
COPY --from=0 /athena/build/submodules/state-transfer/libbyzst.so* /usr/local/lib/

WORKDIR /athena/resources
COPY --from=0 /athena/build/src/athena /athena/athena
COPY --from=0 /athena/build/tools/ath_* /athena/
COPY --from=0 /athena/build/tools/ec* /athena/

WORKDIR /athena
CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    cp /athena/config-public/*.pub /athena/config-local && \
    /athena/config-public/find-docker-instances.sh && \
    /athena/athena -c /athena/config-local/athena.config
# athena<->helen
EXPOSE 5458
# SBFT
EXPOSE 3501/udp
EXPOSE 3502/udp
EXPOSE 3503/udp
EXPOSE 3504/udp
EXPOSE 3505/udp
