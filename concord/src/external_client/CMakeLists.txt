file(GLOB concord_external_client_sources "*.cpp" "*.hpp")

add_library(concord_external_client STATIC
    ${concord_external_client_sources}
    ../config/configuration_manager.cpp
    )

get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)
target_include_directories(concord_external_client PRIVATE ${thresh_include})

target_link_libraries(concord_external_client PUBLIC
    ${Boost_LIBRARIES}
    ${yaml-cpp_LIBRARIES}
    log4cplus
    concord_utils
    bftclient
    kvbc
    threshsign)
