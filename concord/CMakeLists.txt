cmake_minimum_required(VERSION 3.5)
project(concord)

# Require C++17.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS Off)
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")

# Storage options
option(BUILD_ROCKSDB_STORAGE "concord needs rocksdb" TRUE)

#concord flags
# Default USE_LOG4CPP to true
option(USE_LOG4CPP "Enable LOG4CPP" TRUE)
set(CONCORD_LOGGER_NAME "concord.bft")

# Default BUILD_COMM_TCP_PLAIN to FALSE
option(BUILD_COMM_TCP_PLAIN "Enable TCP communication" FALSE)

# Default BUILD_COMM_TCP_TLS to true
option(BUILD_COMM_TCP_TLS "Enable TCP TLS communication" TRUE)

# Our CMake helper modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Expected GRPC and Protobuf installations
list(APPEND CMAKE_PREFIX_PATH "/opt/grpc" "/opt/protobuf")

if(APPLE)
  set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "/usr/local/Cellar/protobuf@3.1/3.1.0")
endif()

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIR})

find_package(GRPC REQUIRED)
include_directories(${GRPC_INCLUDE_DIR})

find_package(Boost 1.65 COMPONENTS system program_options thread filesystem
        REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

find_package(jaegertracing CONFIG REQUIRED)
find_package(prometheus-cpp CONFIG REQUIRED)
#find_package(Log4Cplus REQUIRED)
configure_file(test/resources/log4cplus.properties test/resources/log4cplus.properties COPYONLY)
configure_file(test/resources/metrics_config.yaml test/resources/metrics_config.yaml COPYONLY)
configure_file(test/resources/concord1.config test/resources/concord1.config COPYONLY)
configure_file(test/resources/concord2.config test/resources/concord2.config COPYONLY)
configure_file(test/resources/concord3.config test/resources/concord3.config COPYONLY)
configure_file(test/resources/concord4.config test/resources/concord4.config COPYONLY)
configure_file(test/resources/external_client_tls.config test/resources/external_client_tls.config COPYONLY)
configure_file(test/resources/external_client_udp.config test/resources/external_client_udp.config COPYONLY)
file(COPY test/resources/tls_certs DESTINATION test/resources)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
if(DEFINED USE_HERA)
  find_package(Hera REQUIRED)
  include_directories(${Hera_INCLUDE_DIRS})
  set(EVM_LIBRARIES ${Hera_LIBRARIES})
else(DEFINED USE_HERA)
  find_package(Evmjit REQUIRED)
  include_directories(${Evmjit_INCLUDE_DIRS})
  set(EVM_LIBRARIES ${Evmjit_LIBRARIES})

  if(APPLE)
    set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "/usr/local/Cellar/llvm@5/5.0.2/lib/cmake/llvm")
  endif(APPLE)
  find_package(LLVM REQUIRED CONFIG)
endif(DEFINED USE_HERA)

find_package(cryptopp REQUIRED)

find_package(yaml-cpp REQUIRED)
include_directories(${yaml-cpp_INCLUDE_DIRS})

add_subdirectory("submodules")
add_subdirectory("proto")
add_subdirectory("src")
add_subdirectory("tools")
add_subdirectory("scripts")

## Unit tests with googletest
find_package(GTEST REQUIRED)
add_subdirectory("test")
enable_testing()
