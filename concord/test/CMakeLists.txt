include_directories(
  ${GTEST_INCLUDE_DIR}
  ${PROJECT_SOURCE_DIR}/src
  )

message("test including " ${GTEST_LIBRARY})
message("test including " ${GTEST_MAIN_LIBRARY})

enable_testing()
add_test(NAME UtilsTests COMMAND UtilsTests)
add_test(NAME SignTests COMMAND SignTests)
add_test(NAME SliverTests COMMAND SliverTests)
add_test(NAME ConfigTests COMMAND ConfigTests)
add_test(NAME MultiIOTests COMMAND MultiIOTests)
add_test(NAME MetadataStorageTests COMMAND MetadataStorageTests)


add_executable(UtilsTests
  utils_test.cpp)

configure_file(resources/genesis.json resources/genesis.json COPYONLY)

target_link_libraries(UtilsTests
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

add_executable(ConfigTests
  config_test.cpp
  "../src/configuration_manager.cpp")

target_link_libraries(ConfigTests
  ${Boost_LIBRARIES}
  ${GTEST_LIBRARIES}
  concord_utils
  kvblockchain
  gtest
  gtest_main)

add_executable(MultiIOTests
  multiIO_test.cpp)

target_link_libraries(MultiIOTests
  ${GTEST_LIBRARIES}
  kvblockchain
  gtest
  gtest_main)

add_executable(MetadataStorageTests
  metadataStorage_test.cpp)

target_link_libraries(MetadataStorageTests
  ${GTEST_LIBRARIES}
  kvblockchain
  gtest
  gtest_main)
