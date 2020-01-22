# The base path of the concord-communcation module, where we're keeping
# shared resources for services that communicate with concord nodes.
set(COMMUNICATION_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../communication")
set(PROTOBUF_IMPORT_DIRS ${COMMUNICATION_PATH}/src/main/proto/)

set(DAML_PROTO
  daml_commit.proto
  daml_data.proto
  daml_events.proto
  daml_validator.proto
)

set(HLF_PROTO
  hlf_services.proto
  hlf_storage.proto
)

set(THIN_REPLICA_PROTO ${COMMUNICATION_PATH}/src/main/proto/thin_replica.proto)

set(TEE_PROTO tee.proto)

protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS
  ${CMAKE_CURRENT_BINARY_DIR}
  ${COMMUNICATION_PATH}/src/main/proto/concord.proto
  concord_storage.proto
  ${DAML_PROTO}
  ${HLF_PROTO}
  ${THIN_REPLICA_PROTO}
  ${TEE_PROTO}
)

grpc_generate_cpp(GRPC_SRCS GRPC_HDRS ${CMAKE_CURRENT_BINARY_DIR}
  ${DAML_PROTO}
  ${HLF_PROTO}
  ${THIN_REPLICA_PROTO}
  ${TEE_PROTO}
)

add_library(proto STATIC ${PROTO_SRCS} ${PROTO_HDRS} ${GRPC_SRCS} ${GRPC_SRCS})
target_link_libraries(proto protobuf::libprotobuf gRPC::grpc++)
target_include_directories(proto PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
