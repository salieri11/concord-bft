# Copyright 2020 VMware, all rights reserved
# Dockerfile for building the concord operator image

## Build image
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v20 AS build

## Install gtest
RUN make -C /googletest/_build install

WORKDIR /concord
COPY ./concord /concord
COPY ./communication/src/main/proto /communication/src/main/proto

WORKDIR /concord/build
RUN cmake -DCMAKE_C_COMPILER=/usr/bin/clang-7 \
    -DCMAKE_CXX_COMPILER=/usr/bin/clang++-7 \
    -DCMAKE_CXX_FLAGS="-march=x86-64 -mtune=generic" .. && \
    make operator_server


## Base Run image
FROM ubuntu:18.04
LABEL Description="Concord Operator"

RUN apt-get update && apt-get --no-install-recommends -y install \
    libboost-atomic1.65.1 \
    libboost-chrono1.65.1 \
    libboost-date-time1.65.1 \
    libboost-filesystem1.65.1 \
    libboost-program-options1.65.1 \
    libboost-system1.65.1 \
    libboost-thread1.65.1 \
    libyaml-cpp0.5v5 \
    lldb \
    vim \
    python3 \
    python3-pip \
    && pip3 install requests \
    && rm -rf /var/lib/apt/lists/*


COPY --from=build /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/
RUN ln -s /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/liblog4cplus-2.0.so.3
RUN ln -s /usr/local/lib/liblog4cplus-2.0.so.3.4.4 /usr/local/lib/liblog4cplus.so


COPY --from=build /usr/local/lib/libprometheus-cpp-core.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-core.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

COPY --from=build /usr/local/lib/libprometheus-cpp-pull.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-pull.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

COPY --from=build /usr/local/lib/libprometheus-cpp-push.so.0.8.0 /usr/local/lib/
RUN for x in /usr/local/lib/libprometheus-cpp-push.so.0.8.0; do \
    ln -s $x ${x%%.0} && \
    ln -s $x ${x%%.0.8.0}; \
    done

COPY --from=build /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so.1
RUN ln -s /usr/local/lib/libopentracing.so.1.5.0 /usr/local/lib/libopentracing.so

WORKDIR /operator
COPY --from=build /concord/build/src/operator/operator_server /operator
COPY --from=build /concord/src/operator/concop /operator

ENTRYPOINT ["/operator/operator_server"]
