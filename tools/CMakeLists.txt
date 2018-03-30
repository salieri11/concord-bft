set(ATH_CMD_UTILS
    "athcmdconn.cpp" "athcmdfmt.cpp" "athcmdopt.cpp")

add_executable(ath_sendtx
               "ath_sendtx.cpp"
               ${ATH_CMD_UTILS}
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_sendtx
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})

add_executable(ath_gettxrcpt
               "ath_gettxrcpt.cpp"
               ${ATH_CMD_UTILS}
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_gettxrcpt
                      proto ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})
