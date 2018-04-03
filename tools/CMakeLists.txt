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
