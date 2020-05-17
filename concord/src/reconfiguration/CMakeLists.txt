file(GLOB concord_reconfiguration_sources "*.cpp")

add_library(concord_reconfiguration ${concord_reconfiguration_sources})

target_link_libraries(concord_reconfiguration PUBLIC concord_consensus proto util concordbft_storage corebft kvbc)
