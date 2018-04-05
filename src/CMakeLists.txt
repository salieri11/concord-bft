file(GLOB athena_src "*.cpp" "*.hpp")

add_subdirectory("common")

add_executable(athena
  ${athena_src}
  ${PROTO_SRCS} ${PROTO_HDRS})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(athena
  proto ${PROTOBUF_LIBRARIES}
  ${Boost_LIBRARIES}
  log4cplus
  ${EVM_LIBRARIES}
  ${Cryptopp_LIBRARIES}
  utils)
