file(GLOB athena_src "*.cpp" "*.hpp")

string(APPEND CMAKE_CXX_FLAGS " -Wall")
if(NOT APPLE)
string(APPEND CMAKE_CXX_FLAGS " -Werror")
endif()

add_subdirectory("common")
add_subdirectory("kvb")

add_executable(athena
  ${athena_src}
  ${PROTO_SRCS} ${PROTO_HDRS})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

if(USE_ROCKSDB)
target_compile_definitions(athena PRIVATE USE_ROCKSDB=1)
endif()

target_compile_definitions(athena PRIVATE __BASE=1 SPARSE_STATE=1 USE_TLS=1)

target_link_libraries(athena
  proto ${PROTOBUF_LIBRARIES}
  ${Boost_LIBRARIES}
  log4cplus
  ${EVM_LIBRARIES}
  ${Cryptopp_LIBRARIES}
  LLVMSupport
  athena_utils
  kvblockchain
  secp256k1
)
