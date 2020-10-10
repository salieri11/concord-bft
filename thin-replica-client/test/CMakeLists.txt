include_directories(
  ${GTEST_INCLUDE_DIR}
  ${GMOCK_INCLUDE_DIR}
  ${PROJECT_SOURCE_DIR}/src)

enable_testing()

add_test(NAME ThinReplicaClientTests COMMAND ThinReplicaClientTests)
add_executable(ThinReplicaClientTests
               thin_replica_client_test.cpp
	       thin_replica_client_mocks.hpp
	       thin_replica_client_mocks.cpp)
target_link_libraries(ThinReplicaClientTests
                      ${GTEST_LIBRARY}
		      ${GMOCK_LIBRARY}
		      thin_replica_client)

add_test(NAME TrcBasicUpdateQueueTests COMMAND TrcBasicUpdateQueueTests)
add_executable(TrcBasicUpdateQueueTests
               trc_basic_update_queue_test.cpp)
target_link_libraries(TrcBasicUpdateQueueTests
                      ${GTEST_LIBRARY}
		      thin_replica_client)

add_test(NAME TrcHashTests COMMAND TrcHashTests)
add_executable(TrcHashTests trc_hash_test.cpp)
target_link_libraries(TrcHashTests
                      ${GTEST_LIBRARY}
                      thin_replica_client)

