# The base path of the concord-communcation module, where we're keeping
# shared resources for services that communicate with concord nodes.
set(COMMUNICATION_PATH "../../communication")

protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS
  "${COMMUNICATION_PATH}/src/main/proto/concord.proto"
  concord_storage.proto)

add_library(proto STATIC ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(proto ${PROTOBUF_LIBRARIES})
target_include_directories(proto PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
