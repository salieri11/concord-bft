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
  utils
  gtest
  gtest_main
  pthread)
