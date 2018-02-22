# Build Front End image
FROM node:8.9.4
LABEL Description="Build environment for P2_Blockchain UI"

RUN npm install -g @angular/cli --unsafe

WORKDIR /helen

COPY ./webapp /helen
RUN npm install
RUN npm run build

## Build image
FROM ubuntu:latest
LABEL Description="Build environment for P2_Blockchain"

RUN apt-get update && apt-get -y install \
    cmake \
    g++ \
    git \
    erlang-asn1 \
    erlang-base \
    erlang-common-test \
    erlang-crypto \
    erlang-debugger \
    erlang-dev \
    erlang-dialyzer \
    erlang-et \
    erlang-eunit \
    erlang-inets \
    erlang-mnesia \
    erlang-mode \
    erlang-observer \
    erlang-public-key \
    erlang-runtime-tools \
    erlang-snmp \
    erlang-ssh \
    erlang-ssl \
    erlang-syntax-tools \
    erlang-test-server \
    erlang-tools \
    erlang-webtool \
    erlang-wx \
    erlang-xmerl \
    && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/weidai11/cryptopp.git
WORKDIR /cryptopp/build
RUN git checkout CRYPTOPP_5_6_5
RUN cmake .. && make && make install

WORKDIR /helen
COPY . /helen
COPY --from=0 /helen/dist /helen/priv/www/assets
RUN mv /helen/priv/www/assets/index.html /helen/priv/www
RUN mv /helen/priv/www/assets/favicon.ico /helen/priv/www
RUN export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    make all && \
    ./rebar3 as prod release

## Run image
FROM ubuntu:latest
LABEL Description="P2_Blockchain"

RUN apt-get update && apt-get -y install \
    erlang-asn1 \
    erlang-base \
    erlang-crypto \
    erlang-debugger \
    erlang-et \
    erlang-inets \
    erlang-mnesia \
    erlang-public-key \
    erlang-runtime-tools \
    erlang-snmp \
    erlang-ssh \
    erlang-ssl \
    erlang-syntax-tools \
    erlang-test-server \
    erlang-tools \
    erlang-xmerl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=1 /usr/local/lib/libcryptopp* /usr/local/lib/
COPY --from=1 /helen/_build/prod/rel/helen /helen

CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    /helen/bin/helen console

EXPOSE 8080
