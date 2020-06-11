# NOTE: The original name GTEST causes to problems on osfx since it is case insensitive
# Before monorepo, googletest was located at ../googletest. After
# monorepo, it's located at ../../googletest ... except in Docker
# building, where it's still at ../googletest. (TODO)
find_path(GTEST_INCLUDE_DIR gtest/gtest.h
  HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../googletest/googletest/include
  ${CMAKE_CURRENT_SOURCE_DIR}/../../googletest/googletest/include)

find_path(GMOCK_INCLUDE_DIR gmock/gmock.h
        HINTS ${CMAKE_CURRENT_SOURCE_DIR}/../googletest/googlemock/include
        ${CMAKE_CURRENT_SOURCE_DIR}/../../googletest/googlemock/include)

if(DEFINED GTEST_INCLUDE_DIR_NOTFOUND)
  message(SEND_ERROR "GTEST not found")
else(DEFINED GTEST_INCLUDE_DIR_FOUND)
  message(STATUS "GTEST found at " ${GTEST_INCLUDE_DIR})
  set(GTEST_FOUND 1)

  set(GTEST_DIR "${GTEST_INCLUDE_DIR}/../../_build/googlemock/gtest")
  set(GMOCK_DIR "${GTEST_INCLUDE_DIR}/../../_build/googlemock")

  find_library(GTEST_LIBRARY
          NAMES gtest
          libgtest
          libgtest.a
          PATHS "${GTEST_DIR}")

  find_library(GMOCK_LIBRARY
          NAMES gmock
          libgmock
          libgmock.a
          PATHS "${GMOCK_DIR}")

  find_library(GTEST_LIBRARY_MAIN
          NAMES gtest_main
          libgtest_main
          libgtest_main.a
          PATHS "${GTEST_DIR}")

endif(DEFINED GTEST_INCLUDE_DIR_NOTFOUND)
