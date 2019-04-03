add_library(concord_utils STATIC
  "concord_utils.cpp" "concord_utils.hpp"
  "rlp.cpp" "rlp.hpp"
  "concord_eth_hash.cpp" "concord_eth_hash.hpp"
  "concord_eth_sign.cpp" "concord_eth_sign.hpp")
install (FILES utils.hpp rlp.hpp concord_eth_hash.hpp concord_eth_sign.hpp
  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
