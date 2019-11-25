# Copyright 2019 VMware, all rights reserved

cmake_minimum_required(VERSION 3.10)

# Generate Protobufs
set(THIN_REPLICA_PROTO "/proto/thin_replica.proto")
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${CMAKE_CURRENT_BINARY_DIR}
  ${THIN_REPLICA_PROTO}
)
grpc_generate_cpp(GRPC_SRCS GRPC_HDRS ${CMAKE_CURRENT_BINARY_DIR}
  ${THIN_REPLICA_PROTO}
)
message(STATUS "gRPC/protobuf generated - see " ${CMAKE_CURRENT_BINARY_DIR})

# Thin Replica Protobuf Library
add_library(proto STATIC ${PROTO_SRCS} ${PROTO_HDRS} ${GRPC_SRCS} ${GRPC_SRCS})
target_link_libraries(proto protobuf::libprotobuf)

# Build the Thin Replica Client library
include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_library(thin_replica_client STATIC
  "thin_replica_client.cpp" "thin_replica_client.hpp")
target_link_libraries(thin_replica_client
  proto
  log4cplus
)
