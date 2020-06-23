################################################################
# A minimal CMake file that is compatible with sbt-jni         #
#                                                              #
# All settings required by sbt-jni have been marked so, please #
# add/modify/remove settings to build your specific library.   #
################################################################

cmake_minimum_required(VERSION 2.8.0)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../../thin-replica-client-native/src/cmake")

# Define project and related variables
# (required by sbt-jni) please use semantic versioning
#
project (bft-client-native)
set(PROJECT_VERSION_MAJOR 0)
set(PROJECT_VERSION_MINOR 0)
set(PROJECT_VERSION_PATCH 1)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Setup JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
    message (STATUS "JNI include directories: ${JNI_INCLUDE_DIRS}")
endif()

# Include directories
include_directories(.)
include_directories(include)
include_directories(${JNI_INCLUDE_DIRS})
include_directories(/opt/protobuf/include)

find_library(BFT_CLIENT NAMES concord_external_client_shared)
if(${BFT_CLIENT} STREQUAL "BFT_CLIENT-NOTFOUND")
      message(SEND_ERROR "Unable to find BFT client lib")
else()
      message(STATUS "BFT client lib found at ${BFT_CLIENT}")
endif()

find_path(BFT_CLIENT_DIR "external")
if((NOT BFT_CLIENT_DIR) OR (NOT EXISTS ${BFT_CLIENT_DIR}))
      message(SEND_ERROR "Unable to find BFT client includes")
else()
      message(STATUS "BFT client includes found at ${BFT_CLIENT_DIR}")
endif()

# Sources
file(GLOB LIB_SRC
  "*.c"
  "*.cc"
  "*.cpp"
)

# Setup installation targets
# (required by sbt-jni) major version should always be appended to library name

set (LIB_NAME ${PROJECT_NAME}${PROJECT_VERSION_MAJOR})
add_library(${LIB_NAME} SHARED ${LIB_SRC})
target_link_libraries(${LIB_NAME} ${BFT_CLIENT})
target_include_directories(${LIB_NAME} PUBLIC ${BFT_CLIENT_DIR})
install(TARGETS ${LIB_NAME} LIBRARY DESTINATION . ARCHIVE DESTINATION .)