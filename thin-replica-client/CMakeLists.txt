# Copyright 2019 VMware, all rights reserved

cmake_minimum_required(VERSION 3.10)
project(thin_replica_client)

# CMake helper modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Expected GRPC and Protobuf installations
list(APPEND CMAKE_PREFIX_PATH "/opt/grpc" "/opt/protobuf")

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIR})

find_package(GRPC REQUIRED)
include_directories(${GRPC_INCLUDE_DIR})

add_subdirectory("lib")
add_subdirectory("scripts")

# Library header
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/lib)
# Generated gRPC header
include_directories(${CMAKE_CURRENT_BINARY_DIR}/lib)

add_subdirectory("example")
add_subdirectory("test-app")

# Unit tests
find_package(GTEST REQUIRED)
add_subdirectory("test")
enable_testing()
