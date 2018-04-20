add_library(utils STATIC
  "utils.cpp" "utils.hpp"
  "rlp.cpp" "rlp.hpp")
install (FILES utils.hpp rlp.hpp DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
