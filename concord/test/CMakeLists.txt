include_directories(
  ${GTEST_INCLUDE_DIR}
  ${PROJECT_SOURCE_DIR}/src
  )

message("test including " ${GTEST_LIBRARY})
message("test including " ${GTEST_MAIN_LIBRARY})

add_executable(Tests
  utils_test.cpp)

configure_file(resources/genesis.json resources/genesis.json COPYONLY)

target_link_libraries(Tests
  ${GTEST_LIBRARIES}
  concord_utils
  gtest
  gtest_main
  pthread)

add_executable(SignTests
  signature_test.cpp)

target_link_libraries(SignTests
  ${GTEST_LIBRARIES}
  concord_utils
  gtest
  gtest_main
  pthread
  log4cplus
  secp256k1
  ${Cryptopp_LIBRARIES})

add_executable(SliverTests
  sliver_test.cpp)

target_link_libraries(SliverTests
  ${GTEST_LIBRARIES}
  concord_utils
  kvblockchain
  gtest
  gtest_main)
