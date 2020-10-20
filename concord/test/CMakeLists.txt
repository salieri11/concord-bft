add_subdirectory("external_client")

include_directories(
        ${GTEST_INCLUDE_DIR}
        ${GMOCK_INCLUDE_DIR}
        ${PROJECT_SOURCE_DIR}/src
        ../build/proto/
)

message("test including " ${GTEST_LIBRARY})
message("test including " ${GTEST_MAIN_LIBRARY})
message("test including " ${GMOCK_LIBRARY})

enable_testing()
add_test(NAME UtilsTests COMMAND UtilsTests)
add_test(NAME SignTests COMMAND SignTests)
add_test(NAME ConfigTests COMMAND ConfigTests)
add_test(NAME YamlMergeTest COMMAND YamlMergeTest)
add_test(NAME ReplicaStateSyncTests COMMAND ReplicaStateSyncTests)
add_test(NAME TimeContractTests COMMAND TimeContractTests)
add_test(NAME DamlTest COMMAND DamlTest)
add_test(NAME PruningSMTest COMMAND PruningSMTest)
add_test(NAME ReconfigurationSMTest COMMAND ReconfigurationSMTest)
add_test(NAME KVBFilterTest COMMAND KVBFilterTest)
if(EXTERNAL_CLIENTS_TEST)
  add_test(NAME ExternalClientTest COMMAND ExternalClientTest)
endif()
add_test(NAME ThinReplicaTest COMMAND ThinReplicaTest)
add_test(NAME TrsSubBufferTest COMMAND TrsSubBufferTest)
add_test(NAME OpenSSLCryptoWrapperTest COMMAND OpenSSLCryptoWrapperTest)

add_executable(agent_test_server
        agent_test_server.cpp)

target_link_libraries(agent_test_server
  logging
  Threads::Threads
  nlohmann_json::nlohmann_json
)

add_executable(UtilsTests
        utils_test.cpp)

configure_file(resources/genesis.json resources/genesis.json COPYONLY)

target_link_libraries(UtilsTests
        ${GTEST_LIBRARY}
        concord_utils
        pthread
        ${CRYPTOPP_LIBRARIES}
        logging)

add_executable(SignTests
        signature_test.cpp)

target_link_libraries(SignTests
        ${GTEST_LIBRARY}
        concord_utils
        pthread
        secp256k1
        ${CRYPTOPP_LIBRARIES}
        logging)

add_executable(ConfigTests
        config_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(ConfigTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        concord_utils
        concord_consensus
        logging
        )

add_executable(ReplicaStateSyncTests
        ../src/config/configuration_manager.cpp
        ../src/storage/concord_block_metadata.cpp
        replicaStateSync_test.cpp
        )

target_link_libraries(ReplicaStateSyncTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        logging
        concord_storage
        kvbc)

add_executable(TimeContractTests
        time_contract_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(TimeContractTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        logging
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_time
        )

add_executable(DamlTest
        daml_test.cpp
        daml_validator_client_test.cpp
        mocks.hpp
        mocks.cpp
        ../src/daml/daml_kvb_commands_handler.cpp
        ../src/daml/daml_validator_client.cpp
        ../src/config/configuration_manager.cpp
        )

target_link_libraries(DamlTest
        ${GTEST_LIBRARY}
        ${GMOCK_LIBRARY}
        ${Boost_LIBRARIES}
        concord_consensus
        concord_utils
        concord_common
        proto
        logging
        )

add_executable(PruningSMTest
        pruning_sm_test.cpp
        mocks.hpp
        mocks.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(PruningSMTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${GMOCK_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_pruning
        logging)

add_executable(ReconfigurationSMTest
		mocks.cpp
		../src/config/configuration_manager.cpp
        reconfiguration_sm_test.cpp)

target_link_libraries(ReconfigurationSMTest
		${Boost_LIBRARIES}
		${GTEST_LIBRARY}
		${yaml-cpp_LIBRARIES}
		proto
		cmf_messages
		concord_utils
		concord_common
		concord_reconfiguration
		logging)

add_executable(ThinReplicaTest
        thin_replica_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(ThinReplicaTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_trs
        logging)

add_executable(TrsSubBufferTest
        trs_sub_buffer_test.cpp)
target_include_directories(TrsSubBufferTest PUBLIC
        ${PROJECT_SOURCE_DIR}/submodules/concord-bft/kvbc/include)
target_link_libraries(TrsSubBufferTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        concord_utils
        concord_common
        logging)

add_executable(KVBFilterTest
        kvbfilter_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(KVBFilterTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_pruning
        logging)

add_executable(ExternalClientTest external_client_test.cpp)

get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)
target_include_directories(ExternalClientTest PRIVATE ${thresh_include})

target_link_libraries(ExternalClientTest
        ${GTEST_LIBRARY}
        concord_external_client
        logging)

add_executable(OpenSSLCryptoWrapperTest
               openssl_crypto_wrapper_tests.cpp)
target_link_libraries(OpenSSLCryptoWrapperTest
                      ${GTEST_LIBRARY}
		                  concord_utils
                      ${CRYPTOPP_LIBRARIES}
		                  pthread
                      logging)

add_executable(YamlMergeTest
        yaml_merge_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(YamlMergeTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        concord_utils
        concord_consensus
        logging
        )
