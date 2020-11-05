# Future open-source library
add_library(concord_reconfiguration_open
        pruning/pruning_serialization.cpp
        pruning/rsa_pruning_signer.cpp
        pruning/rsa_pruning_verifier.cpp
        pruning/kvb_pruning_sm.cpp
        reconfiguration_sm_open.cpp
        reconfiguration_sm_dispatcher.cpp)
target_include_directories(concord_reconfiguration_open PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(concord_reconfiguration_open PUBLIC
 cmf_messages
 concord_consensus
 concordbft_storage
 corebft
 kvbc
 proto
 concord_utils
 util
 concord_time
 kvbc
)

# Future closed-source library
add_library(concord_reconfiguration reconfiguration_sm.cpp)
target_include_directories(concord_reconfiguration PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(concord_reconfiguration PUBLIC
 concord_reconfiguration_open
)
