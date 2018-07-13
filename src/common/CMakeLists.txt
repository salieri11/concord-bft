add_library(athena_utils STATIC
  "utils.cpp" "utils.hpp"
  "rlp.cpp" "rlp.hpp"
  "athena_eth_hash.cpp" "athena_eth_hash.hpp"
  "athena_eth_sign.cpp" "athena_eth_sign.hpp")
install (FILES utils.hpp rlp.hpp athena_eth_hash.hpp athena_eth_sign.hpp
  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
