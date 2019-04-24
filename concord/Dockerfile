## IMPORTANT: You must have initialized and updated the submodules
## before building the docker image! See README.md.

## ALSO IMPORTANT: You must pass the parent directory as the path for
## this build. We need to copy files from ../communication, and Docker
## will not allow copying files from outside the build scope. Use either:
##
## from this directory: docker build -f Dockerfile ..
##         from parent: docker build -f concord/Dockerfile .

## Build image
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v5

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
RUN cmake -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && CTEST_OUTPUT_ON_FAILURE=true make -j$(nproc) all test


## Base Run image
FROM ubuntu:18.04
LABEL Description="Concord"

ARG memleak_util=""
ARG memleak_util_cmd=""

# Get this variable into the docker image for the CMD.
ENV memleak_util_cmd="${memleak_util_cmd}"

RUN apt-get update \
    && apt-get -y install ${memleak_util} \
    bind9-host \
    libbz2-1.0 \
    libgmp10 \
    liblz4-1 \
    libprotobuf10 \
    libsnappy1v5 \
    libzstd1 \
    llvm-5.0 \
    dnsutils \
    libboost-program-options1.65.1 \
    libboost-system1.65.1 \
    libboost-thread1.65.1 \
    libyaml-cpp0.5v5 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/local/lib/libcryptopp* /usr/local/lib/
COPY --from=0 /usr/local/lib/liblog4cplus* /usr/local/lib/
# evmjit is statically compiled, so we don't need to copy
COPY --from=0 /usr/local/lib/librelic* /usr/local/lib/
COPY --from=0 /usr/local/lib/librocksdb.* /usr/local/lib/
COPY --from=0 /usr/local/lib/libsecp256k1* /usr/local/lib/

WORKDIR /concord/resources
COPY --from=0 /concord/build/src/concord /concord/concord
COPY --from=0 /concord/build/tools/conc_* /concord/
COPY --from=0 /concord/build/tools/ec* /concord/

# Ethereum RPC port.
EXPOSE 5458

# SBFT ports.
EXPOSE 3501/udp
EXPOSE 3502/udp
EXPOSE 3503/udp
EXPOSE 3504/udp
EXPOSE 3505/udp

# Mountable volumes.
VOLUME [ "/concord/config", "/concord/rocksdbdata", "/concord/log" ]

# Set working directory and start.
WORKDIR /concord
CMD export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib && \
    cp /concord/config-local/concord_with_hostnames.config \
       /concord/config-local/concord.config && \
    /concord/config-public/find-docker-instances.sh && \
    ${memleak_util_cmd} \
    /concord/concord -c /concord/config-local/concord.config
