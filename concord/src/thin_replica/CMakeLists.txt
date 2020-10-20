file(GLOB concord_trs_src "grpc_services.cpp")
add_library(concord_trs ${concord_trs_src})
target_include_directories(concord_trs PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(concord_trs PUBLIC
  proto
  kvbc
  logging
  util
)
