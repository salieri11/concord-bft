include_directories(
  ${GTEST_INCLUDE_DIR}
  ${GMOCK_INCLUDE_DIR}
  ${PROJECT_SOURCE_DIR}/src)

enable_testing()
add_test(NAME ThinReplicaClientTests COMMAND ThinReplicaClientTests)

add_executable(ThinReplicaClientTests
               thin_replica_client_test.cpp
	       thin_replica_client_mocks.hpp)
target_link_libraries(ThinReplicaClientTests
                      ${GTEST_LIBRARY}
		      ${GMOCK_LIBRARY}
		      thin_replica_client)
