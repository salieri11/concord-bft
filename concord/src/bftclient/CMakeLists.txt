project (bftclient LANGUAGES CXX)

add_library(bftclient2 STATIC
        src/quorums.cpp
        )

target_include_directories(bftclient2 PUBLIC include)
target_include_directories(bftclient2 PRIVATE src)
target_link_libraries(bftclient2 PUBLIC bftcommunication bftheaders)