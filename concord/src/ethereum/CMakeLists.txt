file(GLOB concord_ethereum_src "*.cpp" "*.hpp")

add_library(concord_ethereum ${concord_ethereum_src})

target_link_libraries(concord_ethereum PUBLIC
  ${EVM_LIBRARIES}
  concord_common
  concord_consensus
  LLVMSupport
  proto
  secp256k1
  ${Cryptopp_LIBRARIES})

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
