set(concord_blockchain_sources
  kvb_storage.cpp kvb_storage.hpp)

add_library(concord_blockchain ${concord_blockchain_sources})

target_link_libraries(concord_blockchain PUBLIC
  concord_common kvblockchain proto)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
