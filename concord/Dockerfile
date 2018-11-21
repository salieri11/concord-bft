## IMPORTANT: You must have initialized and updated the submodules
## before building the docker image! See README.md.

## Build image
FROM vmwblockchain/concord-core:prereqs-v1


WORKDIR /concord
COPY . /concord
WORKDIR /concord/build
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && make


## Base Run image
FROM ubuntu:16.04
LABEL Description="Concord"

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
COPY --from=0 /concord/build/submodules/state-transfer/libbyzst.so* /usr/local/lib/

WORKDIR /concord/resources
COPY --from=0 /concord/build/src/concord /concord/concord
COPY --from=0 /concord/build/tools/conc_* /concord/
COPY --from=0 /concord/build/tools/ec* /concord/

WORKDIR /concord
CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    cp /concord/config-public/*.pub /concord/config-local && \
    /concord/config-public/find-docker-instances.sh && \
    /concord/concord -c /concord/config-local/concord.config
# concord<->helen
EXPOSE 5458
# SBFT
EXPOSE 3501/udp
EXPOSE 3502/udp
EXPOSE 3503/udp
EXPOSE 3504/udp
EXPOSE 3505/udp
