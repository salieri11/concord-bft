# pthread dependency
find_package(Threads REQUIRED)
find_package(nlohmann_json REQUIRED)
find_package(cryptopp REQUIRED)

add_executable(operator_server
  main.cpp
  config.cpp
  operations.cpp
)

target_link_libraries(operator_server
  logging
  Threads::Threads
  concord_config
  nlohmann_json::nlohmann_json
  bftclient_new
  bftclient_new
  proto
  concord_utils
)
