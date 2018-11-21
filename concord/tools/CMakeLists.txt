string(APPEND CMAKE_CXX_FLAGS " -Wall")
if(NOT APPLE)
string(APPEND CMAKE_CXX_FLAGS " -Werror")
endif()

add_library(concmdutil STATIC
            "concmdconn.cpp" "concmdconn.hpp"
            "concmdfmt.cpp" "concmdfmt.hpp"
            "concmdopt.cpp" "concmdopt.hpp"
            "concmdex.hpp"
            ${PROTO_HDRS})
target_link_libraries(concmdutil proto)

add_executable(conc_sendtx
               "conc_sendtx.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(conc_sendtx
                      concmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(conc_gettxrcpt
               "conc_gettxrcpt.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(conc_gettxrcpt
                      concmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(conc_getstat
               "conc_getstat.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(conc_getstat
                      concmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(conc_getblk
               "conc_getblk.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(conc_getblk
                      concmdutil
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

# ecrecover and ecsign need to find src/common/concord_eth_sign.h
include_directories("../src")

add_executable(ecrecover
  "ecrecover.cpp")
target_link_libraries(ecrecover
                      concmdutil
                      concord_utils
                      log4cplus
                      secp256k1
                      ${Cryptopp_LIBRARIES})

add_executable(ecsign
  "ecsign.cpp")
target_link_libraries(ecsign
                      concmdutil
                      concord_utils
                      log4cplus
                      secp256k1
                      ${Cryptopp_LIBRARIES})
