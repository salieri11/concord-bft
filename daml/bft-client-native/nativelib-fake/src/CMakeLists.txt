cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(BFTClient VERSION 0.1)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include_directories(/usr/local/include)

add_library(concord_external_client_shared SHARED concord_client_pool.cpp)
target_compile_definitions(concord_external_client_shared PUBLIC OBJ)
target_link_libraries(concord_external_client_shared)

install(TARGETS concord_external_client_shared LIBRARY DESTINATION lib)
install(FILES concord_client_pool.hpp DESTINATION include)
