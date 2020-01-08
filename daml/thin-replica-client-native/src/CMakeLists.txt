################################################################
# A minimal CMake file that is compatible with sbt-jni         #
#                                                              #
# All settings required by sbt-jni have been marked so, please #
# add/modify/remove settings to build your specific library.   #
################################################################

cmake_minimum_required(VERSION 2.8.0)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Define project and related variables
# (required by sbt-jni) please use semantic versioning
#
project (thin-replica-client-native)
set(PROJECT_VERSION_MAJOR 0)
set(PROJECT_VERSION_MINOR 1)
set(PROJECT_VERSION_PATCH 0)

# Setup JNI
find_package(JNI REQUIRED)
if (JNI_FOUND)
    message (STATUS "JNI include directories: ${JNI_INCLUDE_DIRS}")
endif()

# Include directories
include_directories(.)
include_directories(include)
include_directories(${JNI_INCLUDE_DIRS})

find_library(THIN_REPLICA_CLIENT "thin_replica_client")
if(${THIN_REPLICA_CLIENT} STREQUAL "THIN_REPLICA_CLIENT-NOTFOUND")
      message(SEND_ERROR "Unable to find thin replica client lib")
else()
      message(STATUS "Thin replica client lib found at ${THIN_REPLICA_CLIENT}")
endif()

find_path(THIN_REPLICA_CLIENT_DIR "thin_replica_client")
if((NOT THIN_REPLICA_CLIENT_DIR) OR (NOT EXISTS ${THIN_REPLICA_CLIENT_DIR}))
      message(SEND_ERROR "Unable to find thin replica client includes")
else()
      message(STATUS "Thin replica client includes found at ${THIN_REPLICA_CLIENT_DIR}")
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
target_link_libraries(${LIB_NAME} ${THIN_REPLICA_CLIENT})
target_include_directories(${LIB_NAME} PUBLIC ${THIN_REPLICA_CLIENT_DIR})
install(TARGETS ${LIB_NAME} LIBRARY DESTINATION . ARCHIVE DESTINATION .)