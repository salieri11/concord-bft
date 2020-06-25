project (bftclient LANGUAGES CXX)

file (GLOB bft_headers "../bftclient/include/bftclient/*.h")
install (FILES ${bft_headers} DESTINATION include/bftclient/include/bftclient)

add_library(bftclient2 STATIC
        src/quorums.cpp
        )

target_include_directories(bftclient2 PUBLIC include)
target_include_directories(bftclient2 PRIVATE src)
target_link_libraries(bftclient2 PUBLIC bftcommunication)