# Copyright 2019 VMware, all rights reserved

cmake_minimum_required(VERSION 3.10)

# Generate Protobufs

set(COMMUNICATION_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../communication")
set(THIN_REPLICA_PROTO ${COMMUNICATION_PATH}/src/main/proto/thin_replica.proto)
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${CMAKE_CURRENT_BINARY_DIR}
  ${THIN_REPLICA_PROTO}
)
grpc_generate_cpp(GRPC_SRCS GRPC_HDRS ${CMAKE_CURRENT_BINARY_DIR}
  ${THIN_REPLICA_PROTO}
)
message(STATUS "gRPC/protobuf generated - see " ${CMAKE_CURRENT_BINARY_DIR})

# Thin Replica Protobuf Library
add_library(proto SHARED ${PROTO_SRCS} ${PROTO_HDRS} ${GRPC_SRCS} ${GRPC_SRCS})
target_link_libraries(proto protobuf::libprotobuf)
install(TARGETS proto DESTINATION lib)

find_package(prometheus-cpp CONFIG REQUIRED)

# Build the Thin Replica Client library
include_directories(${CMAKE_CURRENT_BINARY_DIR})
add_library(thin_replica_client SHARED
  "thin_replica_client.cpp" "thin_replica_client.hpp"
  "thin_replica_client_facade.cpp" "thin_replica_client_facade.hpp"
  "trs_connection.cpp" "trs_connection.hpp"
  "update.hpp")
target_link_libraries(thin_replica_client
  proto
  log4cplus
  jaegertracing
  opentracing
  thrift
  gRPC::grpc++
  prometheus-cpp::core
  prometheus-cpp::pull
)

install(TARGETS thin_replica_client DESTINATION lib)
install(FILES thin_replica_client.hpp thin_replica_client_facade.hpp update.hpp DESTINATION "include/thin_replica_client")

