add_executable(kvbc_micro_benchmark  kvbc_micro_benchmark.cpp )
target_include_directories(kvbc_micro_benchmark PUBLIC "${libkvbc_SOURCE_DIR}/include")
target_link_libraries(kvbc_micro_benchmark PUBLIC kvbc)
