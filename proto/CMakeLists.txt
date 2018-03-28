protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS athena.proto)
add_library(proto STATIC ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(proto ${PROTOBUF_LIBRARIES})
target_include_directories(proto PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
