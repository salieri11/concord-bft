string(APPEND CMAKE_CXX_FLAGS " -Wall")
string(APPEND CMAKE_CXX_FLAGS " -Werror")

add_library(athcmdutil STATIC
            "athcmdconn.cpp" "athcmdconn.hpp"
            "athcmdfmt.cpp" "athcmdfmt.hpp"
            "athcmdopt.cpp" "athcmdopt.hpp"
            "athcmdex.hpp"
            ${PROTO_HDRS})
target_link_libraries(athcmdutil proto)

add_executable(ath_sendtx
               "ath_sendtx.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_sendtx
                      athcmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(ath_gettxrcpt
               "ath_gettxrcpt.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_gettxrcpt
                      athcmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(ath_getstat
               "ath_getstat.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_getstat
                      athcmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(ath_getblk
               "ath_getblk.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_getblk
                      athcmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

include_directories("~/deps/secp256k1/include" "../src")
find_library(SECP256K1_LIBRARY secp256k1 PATHS /home/bfink/deps/secp256k1/.libs)
add_executable(ecrecover
  "ecrecover.cpp")
target_link_libraries(ecrecover
                      athcmdutil
                      athena_utils
                      log4cplus
                      ${Cryptopp_LIBRARIES}
                      ${SECP256K1_LIBRARY})

add_executable(ecsign
  "ecsign.cpp")
target_link_libraries(ecsign
                      athcmdutil
                      athena_utils
                      log4cplus
                      ${Cryptopp_LIBRARIES}
                      ${SECP256K1_LIBRARY})
