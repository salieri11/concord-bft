# Use Google Benchmark as a benchmarking library: https://github.com/google/benchmark
#
# Note: Benchmarks are not officially supported yet and are optional. Use QUIET to
# silence CMake in case Google Benchmark is not installed.
find_package(benchmark QUIET)

if(benchmark_FOUND)
    add_executable(sparse_merkle_benchmark sparse_merkle_benchmark.cpp kvbc_micro_benchmark.cpp)
    target_link_libraries(sparse_merkle_benchmark PUBLIC
        benchmark
        util
        corebft
        kvbc
    )
endif()

add_executable(kvbc_micro_benchmark  kvbc_micro_benchmark.cpp)
target_include_directories(kvbc_micro_benchmark PUBLIC "${libkvbc_SOURCE_DIR}/include"})
target_link_libraries(kvbc_micro_benchmark PUBLIC kvbc)