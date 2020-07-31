file(GLOB concord_config_sources "*.cpp" "*.hpp")

add_library(concord_config ${concord_config_sources})

target_link_libraries(concord_config PUBLIC
    ${yaml-cpp_LIBRARIES}
    ${Boost_LIBRARIES}
    threshsign
)
