add_library(utils STATIC
  "utils.cpp" "utils.hpp")
install (FILES utils.hpp DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
