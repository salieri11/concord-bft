file(GLOB athena_src "*.cpp" "*.hpp")
add_executable(athena
               ${athena_src}
               ${PROTO_SRCS} ${PROTO_HDRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(athena
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES}
                      log4cplus
                      ${EVM_LIBRARIES}
                      ${Cryptopp_LIBRARIES})
