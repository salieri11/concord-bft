file(GLOB concord_pruning_sources "*.cpp" "*.hpp")

add_library(concord_pruning ${concord_pruning_sources})

target_link_libraries(concord_pruning PUBLIC concord_consensus proto concordbft_storage util corebft)
