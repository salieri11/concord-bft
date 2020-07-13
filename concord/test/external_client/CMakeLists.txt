get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)

add_executable(external_client_wrp main.cpp external_client_pool.cpp)
target_link_libraries(external_client_wrp 
                        concord_external_client
                        threshsign
                        proto)

target_include_directories(external_client_wrp PRIVATE
                    "${CMAKE_SOURCE_DIR}/src/external_client"
                    "${CMAKE_SOURCE_DIR}/src"
                    "${CMAKE_SOURCE_DIR}/src/tee"
                    ${bftengine_SOURCE_DIR}
                    "${CMAKE_BINARY_DIR}/proto/"
                    ${thresh_include}
                    )

add_executable(perf_test_tool perf_test_tool.cpp)
target_include_directories(perf_test_tool PRIVATE
        "${CMAKE_SOURCE_DIR}/src/external_client"
        "${CMAKE_SOURCE_DIR}/src"
        "${CMAKE_SOURCE_DIR}/src/tee"
        ${bftengine_SOURCE_DIR}
        "${CMAKE_BINARY_DIR}/proto/"
        ${thresh_include}
        )
target_link_libraries(perf_test_tool
        concord_external_client
        logging
        proto)
