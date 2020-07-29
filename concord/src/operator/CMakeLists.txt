# pthread dependency
find_package(Threads REQUIRED)
find_package(nlohmann_json REQUIRED)

add_executable(operator_server main.cpp)

target_link_libraries(operator_server
  logging
  Threads::Threads
  nlohmann_json::nlohmann_json
)
