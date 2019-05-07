set(concord_time_sources
  time_contract.cpp time_contract.hpp
  time_pusher.cpp time_pusher.hpp
  time_reading.cpp time_reading.hpp)

add_library(concord_time ${concord_time_sources})

target_link_libraries(concord_time PUBLIC kvblockchain proto)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
