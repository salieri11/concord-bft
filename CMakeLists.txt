cmake_minimum_required(VERSION 3.5)
project(athena)

# Require C++11.
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS Off)

# Storage options
set(USE_ROCKSDB False)

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})

find_package(Boost 1.64 COMPONENTS system program_options REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

#find_package(Log4Cplus REQUIRED)
configure_file(resources/log4cplus.properties resources/log4cplus.properties COPYONLY)
configure_file(resources/athena.config resources/athena.config COPYONLY)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
if(DEFINED USE_HERA)
  find_package(Hera REQUIRED)
  include_directories(${Hera_INCLUDE_DIRS})
  set(EVM_LIBRARIES ${Hera_LIBRARIES})
else(DEFINED USE_HERA)
  find_package(Evmjit REQUIRED)
  include_directories(${Evmjit_INCLUDE_DIRS})
  set(EVM_LIBRARIES ${Evmjit_LIBRARIES})
endif(DEFINED USE_HERA)

find_package(Cryptopp REQUIRED)
include_directories(${Cryptopp_INCLUDE_DIRS})

add_subdirectory("proto")
add_subdirectory("src")
add_subdirectory("tools")


## Unit tests with googletest
find_package(GTEST REQUIRED)
add_subdirectory("test")
enable_testing()
