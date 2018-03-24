cmake_minimum_required(VERSION 3.5)
project(athena)

# Require C++11.
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS Off)

find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS src/athena.proto)

find_package(Boost 1.64 COMPONENTS system program_options REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

#find_package(Log4Cplus REQUIRED)
configure_file(resources/log4cplus.properties resources/log4cplus.properties COPYONLY)

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


file(GLOB athena_src "src/*.cpp" "src/*.hpp")
add_executable(athena
               ${athena_src}
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(athena
                      ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES}
                      log4cplus
                      ${EVM_LIBRARIES}
                      ${Cryptopp_LIBRARIES})
add_subdirectory("tools")
