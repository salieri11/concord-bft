file(GLOB concord_common_sources "*.cpp" "*.hpp")

add_library(concord_common ${concord_common_sources})
set_target_properties(concord_common PROPERTIES LINKER_LANGUAGE CXX)

target_link_libraries(concord_common PUBLIC proto corebft concord_utils)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/..)
