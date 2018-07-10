file(GLOB athena_src "*.cpp" "*.hpp")

add_subdirectory("common")
add_subdirectory("kvb")

add_executable(athena
  ${athena_src}
  ${PROTO_SRCS} ${PROTO_HDRS})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

include_directories("~/deps/secp256k1/include" "../src")
find_library(SECP256K1_LIBRARY secp256k1 PATHS /home/bfink/deps/secp256k1/.libs)

if(USE_ROCKSDB)
target_compile_definitions(athena PRIVATE USE_ROCKSDB=1)
endif()

target_link_libraries(athena
  proto ${PROTOBUF_LIBRARIES}
  ${Boost_LIBRARIES}
  log4cplus
  ${EVM_LIBRARIES}
  ${Cryptopp_LIBRARIES}
  LLVMSupport
  athena_utils
  kvblockchain
  ${SECP256K1_LIBRARY}
)
