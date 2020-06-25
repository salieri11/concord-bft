file(GLOB threshsign_headers
		"../../submodules/concord-bft/threshsign/include/threshsign/*.h")
install (FILES ${threshsign_headers} DESTINATION include/threshsign)

add_library(concord_utils STATIC
		"concord_utils.cpp" "concord_utils.hpp"
		"open_tracing_utils.cpp" "open_tracing_utils.hpp"
		"rlp.cpp" "rlp.hpp"
		"concord_eth_hash.cpp" "concord_eth_hash.hpp"
		"concord_eth_sign.cpp" "concord_eth_sign.hpp"
		"concord_prometheus_metrics.hpp" "concord_prometheus_metrics.cpp"
		"openssl_crypto_utils.hpp" "openssl_crypto_utils.cpp"
		)
target_link_libraries(concord_utils secp256k1 util prometheus-cpp::core
		prometheus-cpp::pull yaml-cpp jaegertracing opentracing
		${OPENSSL_LIBRARIES})
target_include_directories(concord_utils PUBLIC ${CRYPTOPP_INCLUDE_DIRS})

install (FILES concord_utils.hpp rlp.hpp concord_eth_hash.hpp concord_eth_sign.hpp
		DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

add_library(concord_utils_shared SHARED
		"concord_utils.cpp" "concord_utils.hpp"
		"open_tracing_utils.cpp" "open_tracing_utils.hpp"
		"rlp.cpp" "rlp.hpp"
		"concord_eth_hash.cpp" "concord_eth_hash.hpp"
		"concord_eth_sign.cpp" "concord_eth_sign.hpp"
		"concord_prometheus_metrics.hpp" "concord_prometheus_metrics.cpp"
		"openssl_crypto_utils.hpp" "openssl_crypto_utils.cpp"
		)
target_link_libraries(concord_utils_shared secp256k1 util_shared prometheus-cpp::core
		prometheus-cpp::pull yaml-cpp jaegertracing opentracing
		${OPENSSL_LIBRARIES} ${CRYPTOPP_LIBRARIES} proto)
install (TARGETS concord_utils_shared DESTINATION lib${LIB_SUFFIX})