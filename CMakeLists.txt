cmake_minimum_required(VERSION 3.5)
project(athena)

# Require C++11.
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS Off)
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")

# Storage options
set(USE_ROCKSDB True)

if(APPLE)
  set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "/usr/local/Cellar/protobuf@2.6/2.6.0")
endif()

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})

find_package(Boost 1.64 COMPONENTS system program_options thread REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

#find_package(Log4Cplus REQUIRED)
configure_file(resources/log4cplus.properties resources/log4cplus.properties COPYONLY)
configure_file(resources/athena1.config resources/athena1.config COPYONLY)
configure_file(resources/athena2.config resources/athena2.config COPYONLY)
configure_file(resources/athena3.config resources/athena3.config COPYONLY)
configure_file(resources/athena4.config resources/athena4.config COPYONLY)
file(COPY resources/sbft
     DESTINATION resources/)

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

find_package(Cryptopp REQUIRED)
include_directories(${Cryptopp_INCLUDE_DIRS})

add_subdirectory("submodules")
add_subdirectory("proto")
add_subdirectory("src")
add_subdirectory("tools")

## Unit tests with googletest
find_package(GTEST REQUIRED)
add_subdirectory("test")
enable_testing()
