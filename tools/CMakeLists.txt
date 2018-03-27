add_executable(ath_sendtx
               "ath_sendtx.cpp"
               ${PROTO_SRCS} ${PROTO_HDRS})
target_link_libraries(ath_sendtx
                      ${PROTOBUF_LIBRARIES}
                      ${Boost_LIBRARIES})
