set(kvblockchain_sources
  blockchain_db_adapter.cpp blockchain_db_adapter.h
  blockchain_db_helpers.cpp
  blockchain_interfaces.h
  client_imp.cpp client_imp.h
  comparators.cpp comparators.h
  database_interface.h
  hash_defs.h
  hex_tools.cpp hex_tools.h
  in_memory_db_client.cpp in_memory_db_client.h
  replica_imp.cpp replica_imp.h
  rocksdb_client.cpp rocksdb_client.h
  rocksdb_metadata_storage.cpp
  status.cpp status.hpp
  sliver.cpp sliver.hpp
  bft_configuration.hpp)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)

add_library(kvblockchain ${kvblockchain_sources})

get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)
target_include_directories(kvblockchain PUBLIC ${thresh_include})

target_link_libraries(kvblockchain PUBLIC corebft)

if(USE_ROCKSDB)
find_library(ROCKSDB rocksdb)
find_library(LIBBZ2 bz2)
find_library(LIBLZ4 lz4)
find_library(LIBZSTD zstd)
find_library(LIBZ z)
find_library(LIBSNAPPY snappy)
target_compile_definitions(kvblockchain PRIVATE USE_ROCKSDB=1)
target_link_libraries(kvblockchain PUBLIC ${ROCKSDB} ${LIBBZ2} ${LIBLZ4} ${LIBZSTD} ${LIBZ} ${LIBSNAPPY})
endif()

target_compile_definitions(kvblockchain PUBLIC __BASE=1 SPARSE_STATE=1 USE_TLS=1)
if(${BUILD_COMM_TCP_TLS})
    target_compile_definitions(kvblockchain PUBLIC USE_COMM_TLS_TCP)
endif()
