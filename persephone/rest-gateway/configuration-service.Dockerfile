# Build time image.
FROM golang as builder

RUN go get -u golang.org/x/sys/unix && \
    go get -u google.golang.org/grpc && \
    go get -u github.com/golang/protobuf/proto && \
    go get -u github.com/golang/protobuf/protoc-gen-go && \
    go get -u github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway && \
    go get -u github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger

RUN apt-get update && apt-get install -y unzip
RUN curl --silent --show-error --location --output protoc.zip \
    https://github.com/google/protobuf/releases/download/v3.7.1/protoc-3.7.1-linux-x86_64.zip \
    && unzip -d /usr/local protoc.zip include/\* bin/\* \
    && rm -f protoc.zip

COPY api/src /build/
COPY rest-gateway/configuration-service/main.go /go/src/main.go

RUN protoc -I/usr/local/include -I/build/protobuf -I$GOPATH/src \
    -I$GOPATH/src/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc:$GOPATH/src/. \
    /build/protobuf/vmware/blockchain/deployment/v1/core.proto \
    /build/protobuf/vmware/blockchain/deployment/v1/concord_model.proto \
    /build/protobuf/vmware/blockchain/deployment/v1/security_identity.proto \
    /build/protobuf/vmware/blockchain/deployment/v1/configuration_service.proto
RUN protoc -I/usr/local/include -I/build/protobuf -I$GOPATH/src \
    -I$GOPATH/src/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc:$GOPATH/src/. \
    /build/protobuf/vmware/blockchain/ethereum/type/genesis.proto
RUN protoc -I/usr/local/include -I/build/protobuf \
    -I$GOPATH/src/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --grpc-gateway_out=logtostderr=true,grpc_api_configuration=/build/resources/configuration_service.yaml:$GOPATH/src/. \
    /build/protobuf/vmware/blockchain/deployment/v1/configuration_service.proto

## Static binary build.
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o main /go/src/main.go

# Runtime image (minimal footprint).
FROM alpine:3.10
LABEL description="VMware Blockchain Fleet Management Configuration Service REST Gateway"

RUN apk add bash

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt

WORKDIR /gateway/persephone-configuration
COPY --from=builder /go/main .

EXPOSE 9083

CMD ["./main", "--grpc-server-endpoint", "localhost:9003"]
