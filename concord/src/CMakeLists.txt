file(GLOB concord_src "main.cpp"
  "api/*.cpp"        "api/*.hpp"
  "blockchain/*.cpp" "blockchain/*.hpp"
  "common/*.cpp"     "common/*.hpp"
  "config/*.cpp"     "config/*.hpp"
  "consensus/*.cpp"  "consensus/*.hpp"
  "ethereum/*.cpp"   "ethereum/*.hpp"
)

string(APPEND CMAKE_CXX_FLAGS " -Wall")
if(NOT APPLE)
string(APPEND CMAKE_CXX_FLAGS " -Werror")
endif()

add_subdirectory("utils")
add_subdirectory("consensus/kvb")

add_executable(concord
  ${concord_src}
  ${PROTO_SRCS} ${PROTO_HDRS})

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
  ${EVM_LIBRARIES}
  ${Cryptopp_LIBRARIES}
  ${yaml-cpp_LIBRARIES}
  LLVMSupport
  concord_utils
  kvblockchain
  secp256k1
)
