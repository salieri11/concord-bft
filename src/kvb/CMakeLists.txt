set(kvblockchain_sources
  BlockchainDBAdapter.cpp BlockchainDBAdapter.h
  BlockchainInterfaces.h
  ClientImp.cpp ClientImp.h
  Comparators.cpp Comparators.h
  DatabaseInterface.h
        HashDefs.h
  HexTools.cpp HexTools.h
  InMemoryDBClient.cpp InMemoryDBClient.h
  ReplicaImp.cpp ReplicaImp.h
  RocksDBClient.cpp RocksDBClient.h
  SimpleThreadPool.h
  slice.cpp slice.h
  bft_configuration.hpp)

add_library(kvblockchain ${kvblockchain_sources})

# TODO(BWF): Files from these three paths are included in kvb/; we
# should migrate any fixes from kvb/ back to P2_Blockchain, and then
# just use them from the submodule
#target_include_directories(kvblockchain PUBLIC ../../submodules/P2_Blockchain/Utils)
#target_include_directories(kvblockchain PUBLIC ../../submodules/P2_Blockchain/Utils/Thread)
#target_include_directories(kvblockchain PUBLIC ../../submodules/P2_Blockchain/Utils/Database)

#target_include_directories(kvblockchain PUBLIC ../.
#./submodules/P2_Blockchain/Utils/Crypto)
#./submodules/P2_Blockchain/AgreementModules/SbftForIntegMay18/libbyz)

#target_link_libraries(kvblockchain PUBLIC byz)
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
