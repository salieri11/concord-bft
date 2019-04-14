set(client_sources
  TesterClient/main.cpp
  basicRandomTestsRunner.cpp
  simpleKVBTestsBuilder.cpp
  tools/KeyfileIOUtils.cpp
  config/test_comm_config.cpp
  config/config_file_parser.cpp
  ${PROJECT_SOURCE_DIR}/src/config/configuration_manager.cpp)

set(replica_sources
  TesterReplica/main.cpp
  TesterReplica/internalCommandsHandler.cpp
  basicRandomTestsRunner.cpp
  simpleKVBTestsBuilder.cpp
  tools/KeyfileIOUtils.cpp
  config/test_comm_config.cpp
  config/config_file_parser.cpp
  ${PROJECT_SOURCE_DIR}/src/consensus/replica_state_sync_imp.cpp
  ${PROJECT_SOURCE_DIR}/src/common/concord_types.cpp
  ${PROJECT_SOURCE_DIR}/src/common/concord_log.cpp
  ${PROJECT_SOURCE_DIR}/src/ethereum/concord_evm.cpp
  ${PROJECT_SOURCE_DIR}/src/ethereum/evm_init_params.cpp
  ${PROJECT_SOURCE_DIR}/src/blockchain/kvb_storage.cpp)

add_executable(skvb_client ${client_sources})
add_executable(skvb_replica ${replica_sources})

target_include_directories(skvb_client PUBLIC
  ${PROJECT_SOURCE_DIR}/src config include)

target_include_directories(skvb_replica PUBLIC
  ${PROJECT_SOURCE_DIR}/src config include)

target_link_libraries(skvb_client
  ${yaml-cpp_LIBRARIES}
  log4cplus
  kvblockchain
  ${Boost_LIBRARIES})

target_link_libraries(skvb_replica
  log4cplus
  proto ${PROTOBUF_LIBRARIES}
  ${EVM_LIBRARIES}
  ${Cryptopp_LIBRARIES}
  LLVMSupport
  concord_utils
  kvblockchain
  secp256k1)

add_custom_target(copy_blockchain_scripts3 ALL COMMENT "Copying scripts abcd")

add_custom_command(TARGET copy_blockchain_scripts3
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/scripts
  ${CMAKE_CURRENT_BINARY_DIR}/scripts)

if (${USE_COMM_PLAIN_TCP})
  target_compile_definitions(skvb_client PUBLIC USE_COMM_PLAIN_TCP)
  target_compile_definitions(skvb_replica PUBLIC USE_COMM_PLAIN_TCP)
endif ()

if (${USE_COMM_TLS_TCP})
  target_compile_definitions(skvb_client PUBLIC USE_COMM_TLS_TCP)
  target_compile_definitions(skvb_replica PUBLIC USE_COMM_TLS_TCP)
endif ()