# Future open-source library
add_library(concord_reconfiguration_open reconfiguration_sm_open.cpp reconfiguration_sm_dispatcher.cpp)
target_include_directories(concord_reconfiguration_open PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(concord_reconfiguration_open PUBLIC
 concord_consensus
 concordbft_storage
 corebft
 kvbc
 proto
 util
)

# Future closed-source library
add_library(concord_reconfiguration reconfiguration_sm.cpp)
target_include_directories(concord_reconfiguration PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(concord_reconfiguration PUBLIC
 concord_reconfiguration_open
)
