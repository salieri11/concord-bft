# Copyright 2020 VMware, all rights reserved
# Dockerfile for building the concord operator image

## Build image
FROM athena-docker-local.artifactory.eng.vmware.com/concord-core:prereqs-v26 AS build

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

COPY --from=0 /opt/protobuf/include /opt/protobuf/include
COPY --from=0 /opt/grpc/include /opt/grpc/include
COPY --from=0 /opt/grpc/bin /opt/grpc/bin

# Copy libraries and create symlinks (docker copies instead of sym linking)
COPY --from=0 /opt/protobuf/lib/*.so.3.13.0.0 /opt/protobuf/lib/
RUN for x in /opt/protobuf/lib/*.so.3.13.0.0; do \
        ln -s $x ${x%%.3.13.0.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.1.32.0 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.1.32.0; do \
        ln -s $x ${x%%.32.0} && \
        ln -s $x ${x%%.1.32.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.12.0.0 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.12.0.0; do \
        ln -s $x ${x%%.0.0} && \
        ln -s $x ${x%%.12.0.0}; \
    done

COPY --from=0 /opt/grpc/lib/*.so.1.2.11 /opt/grpc/lib/
RUN for x in /opt/grpc/lib/*.so.1.2.11; do \
    ln -s $x ${x%%.2.11} && \
    ln -s $x ${x%%.1.2.11}; \
    done

COPY --from=0 /opt/grpc/lib/libre2.so /opt/grpc/lib/

COPY --from=0 /opt/grpc/lib/libabsl_* /opt/grpc/lib/

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/opt/grpc/lib:/opt/protobuf/lib

COPY --from=0 /usr/local/lib/libssl.so.1.1 /usr/local/lib/
RUN ln -s /usr/local/lib/libssl.so.1.1 /usr/local/lib/libssl.so.1
RUN ln -s /usr/local/lib/libssl.so.1.1 /usr/local/lib/libssl.so

COPY --from=0 /usr/local/lib/libcrypto.so.1.1 /usr/local/lib/
RUN ln -s /usr/local/lib/libcrypto.so.1.1 /usr/local/lib/libcrypto.so.1
RUN ln -s /usr/local/lib/libcrypto.so.1.1 /usr/local/lib/libcrypto.so

COPY --from=0 /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/
RUN ln -s /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/libsecp256k1.so.0
RUN ln -s /usr/local/lib/libsecp256k1.so.0.0.0 /usr/local/lib/libsecp256k1.so

COPY --from=0 /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib
RUN ln -s /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib/libhdr_histogram.so.6
RUN ln -s /usr/local/lib/libhdr_histogram.so.6.1.0 /usr/local/lib/libhdr_histogram.so

WORKDIR /operator
COPY --from=build /concord/build/src/operator/operator_server /operator
COPY --from=build /concord/src/operator/concop /operator

ENTRYPOINT ["/operator/operator_server"]
