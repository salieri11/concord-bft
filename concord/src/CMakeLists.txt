file(GLOB concord_src "main.cpp"
  "api/*.cpp"        "api/*.hpp"
  "config/*.cpp"     "config/*.hpp"
  "consensus/*.cpp"  "consensus/*.hpp"
  "daml/*.cpp"       "daml/*.hpp"
)

string(APPEND CMAKE_CXX_FLAGS " -Wall")
if(NOT APPLE)
string(APPEND CMAKE_CXX_FLAGS " -Werror")
endif()

add_subdirectory("utils")
add_subdirectory("common")
add_subdirectory("consensus/kvb")
add_subdirectory("ethereum")
add_subdirectory("time")

add_executable(concord
  ${concord_src}
  ${PROTO_SRCS} ${PROTO_HDRS}
  ${GRPC_SRCS}
)

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

if(USE_ROCKSDB)
target_compile_definitions(concord PRIVATE USE_ROCKSDB=1)
endif()

target_compile_definitions(concord PRIVATE __BASE=1 SPARSE_STATE=1 USE_TLS=1)

target_link_libraries(concord
  proto ${PROTOBUF_LIBRARIES}
  ${Boost_LIBRARIES}
  log4cplus
  ${Cryptopp_LIBRARIES}
  ${yaml-cpp_LIBRARIES}
  concord_utils
  kvblockchain
  secp256k1
  concord_common
  concord_ethereum
  concord_time
  gRPC::grpc++_reflection
  protobuf::libprotobuf
)
