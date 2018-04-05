## Build image
FROM ubuntu:latest
LABEL Description="Build environment for Athena"

RUN apt-get update && apt-get -y install \
    autoconf \
    automake \
    cmake \
    g++ \
    git \
    libprotobuf-dev \
    protobuf-compiler \
    python2.7 \
    wget \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /
RUN git clone https://github.com/google/googletest.git
WORKDIR /googletest/_build
RUN cmake .. && make

WORKDIR /
RUN git clone https://github.com/weidai11/cryptopp.git
WORKDIR /cryptopp/build
RUN git checkout CRYPTOPP_5_6_5
RUN cmake .. && make && make install

WORKDIR /
RUN wget https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.tar.gz \
    && tar -xzf boost_1_64_0.tar.gz \
    && rm boost_1_64_0.tar.gz
WORKDIR /boost_1_64_0
RUN ./bootstrap.sh --with-libraries=system,program_options --prefix=/usr && ./b2 && ./b2 install

WORKDIR /
RUN git clone https://github.com/log4cplus/log4cplus.git
WORKDIR /log4cplus
RUN git checkout REL_1_2_1
RUN sed -i -e "s/am__api_version='1.14'/am__api_version='1.15'/g" configure
RUN ./configure CXXFLAGS="--std=c++11" && make && make install

WORKDIR /
RUN git clone https://github.com/ethereum/evmjit.git
WORKDIR /evmjit
### their interface is evolving rapidly, so use this specific hash
RUN git checkout 4e9f3d76292c7de0c6613427761f843b1719f614
RUN mkdir build
WORKDIR /evmjit/build
RUN cmake ..
RUN cmake --build . --config RelWithDebInfo

WORKDIR /athena
COPY . /athena
WORKDIR /athena/build
RUN cmake .. && make

## Run image
FROM ubuntu:latest
LABEL Description="Athena"

RUN apt-get update && apt-get -y install \
    libprotobuf9v5 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/local/lib/libcryptopp* /usr/local/lib/
COPY --from=0 /usr/lib/libboost* /usr/lib/
COPY --from=0 /usr/local/lib/liblog4cplus* /usr/local/lib/
# evmjit is statically compiled, so we don't need to copy

WORKDIR /athena/resources
COPY --from=0 /athena/build/resources/* /athena/resources/
COPY --from=0 /athena/build/src/athena /athena/athena
COPY --from=0 /athena/build/tools/ath_* /athena/

COPY --from=0 /athena/test/resources/genesis.json /athena/resources/
RUN sed -i -e "s/tmp/athena\/resources/g" /athena/resources/athena.config

WORKDIR /athena
CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    /athena/athena

EXPOSE 5458
