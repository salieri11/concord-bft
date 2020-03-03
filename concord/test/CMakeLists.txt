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
add_test(NAME ReplicaStateSyncTests COMMAND ReplicaStateSyncTests)
add_test(NAME TimeContractTests COMMAND TimeContractTests)
add_test(NAME HlfTest COMMAND HlfTest)
add_test(NAME DamlTest COMMAND DamlTest)
add_test(NAME PruningSMTest COMMAND PruningSMTest)
add_test(NAME KVBFilterTest COMMAND KVBFilterTest)
add_test(NAME ExternalClientTest COMMAND ExternalClientTest)
add_test(NAME ThinReplicaTest COMMAND ThinReplicaTest)

add_executable(UtilsTests
        utils_test.cpp)

configure_file(resources/genesis.json resources/genesis.json COPYONLY)

target_link_libraries(UtilsTests
        ${GTEST_LIBRARY}
        concord_utils
        pthread
        ${CRYPTOPP_LIBRARIES})

add_executable(SignTests
        signature_test.cpp)

target_link_libraries(SignTests
        ${GTEST_LIBRARY}
        concord_utils
        pthread
        log4cplus
        secp256k1
        ${CRYPTOPP_LIBRARIES})

add_executable(ConfigTests
        config_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(ConfigTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        concord_utils
        concord_consensus
        log4cplus
        $<TARGET_OBJECTS:logging_dev>)

add_executable(ReplicaStateSyncTests
        ../src/common/concord_log.cpp
        ../src/common/concord_types.cpp
        ../src/config/configuration_manager.cpp
        ../src/storage/concord_block_metadata.cpp
        ../src/utils/concord_eth_hash.cpp
        ../src/utils/rlp.cpp
        ../src/config/configuration_manager.cpp
        replicaStateSync_test.cpp
        $<TARGET_OBJECTS:logging_dev>)

target_link_libraries(ReplicaStateSyncTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        log4cplus
        proto
        ${EVM_LIBRARIES}
        LLVMSupport
        concord_utils
        concord_storage
        kvbc)

add_executable(TimeContractTests
        time_contract_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(TimeContractTests
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        log4cplus
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_time
        $<TARGET_OBJECTS:logging_dev>)

add_executable(DamlTest
        daml_test.cpp
        mocks.hpp
        mocks.cpp
        ../src/daml/daml_kvb_commands_handler.cpp
        ../src/daml/daml_validator_client.cpp
        ../src/daml/grpc_services.cpp
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
        log4cplus
        $<TARGET_OBJECTS:logging_dev>)

add_executable(HlfTest
        hlf_test.cpp
        ../src/hlf/chaincode_invoker.cpp
        ../src/hlf/kvb_storage.cpp
        ../src/hlf/kvb_commands_handler.cpp
        ../src/config/configuration_manager.cpp
        )

target_link_libraries(HlfTest
        ${GTEST_LIBRARY}
        ${Boost_LIBRARIES}
        ${yaml-cpp_LIBRARIES}
        concord_consensus
        concord_utils
        concord_common
        proto
        log4cplus
        $<TARGET_OBJECTS:logging_dev>)

add_executable(PruningSMTest
        pruning_sm_test.cpp
        mocks.hpp
        mocks.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(PruningSMTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        log4cplus
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_pruning
        $<TARGET_OBJECTS:logging_dev>)

add_executable(ThinReplicaTest
        thin_replica_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(ThinReplicaTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        log4cplus
        proto
        concord_utils
        concord_consensus
        concord_common
        $<TARGET_OBJECTS:logging_dev>)

add_executable(KVBFilterTest
        kvbfilter_test.cpp
        ../src/config/configuration_manager.cpp)

target_link_libraries(KVBFilterTest
        ${Boost_LIBRARIES}
        ${GTEST_LIBRARY}
        ${yaml-cpp_LIBRARIES}
        log4cplus
        proto
        concord_utils
        concord_consensus
        concord_common
        concord_pruning
        $<TARGET_OBJECTS:logging_dev>)

add_executable(ExternalClientTest external_client_test.cpp)

get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)
target_include_directories(ExternalClientTest PRIVATE ${thresh_include})

target_link_libraries(ExternalClientTest
        ${GTEST_LIBRARY}
        concord_external_client
        $<TARGET_OBJECTS:logging_dev>)
