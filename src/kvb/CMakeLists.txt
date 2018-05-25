set(kvblockchain_sources
  BlockchainDBAdapter.cpp BlockchainDBAdapter.h
  BlockchainInterfaces.h
  ClientImp.cpp ClientImp.h
  Comparators.cpp Comparators.h
  DatabaseInterface.h
  Env.cpp
  HashDefs.h
  HexTools.cpp HexTools.h
  InMemoryDBClient.cpp InMemoryDBClient.h
  libbyz.cpp libbyz.h
  ReplicaImp.cpp ReplicaImp.h
  RocksDBClient.cpp RocksDBClient.h
  SimpleThreadPool.h
  slice.cpp slice.h
  Threading.h
  ThreadLocalStorage.h
)

add_library(kvblockchain ${kvblockchain_sources})

# target_link_libraries(kvblockchain LINK_PUBLIC evmutils)
# target_include_directories(kvblockchain PUBLIC .)
# target_include_directories(kvblockchain PUBLIC ../../../Utils)
# target_include_directories(kvblockchain PUBLIC ../../../Utils/Thread)
# target_include_directories(kvblockchain PUBLIC ../../../Utils/Crypto)
# target_include_directories(kvblockchain PUBLIC ../../../Utils/Database)
# target_include_directories(kvblockchain PUBLIC ../../../AgreementModules/SBFT/libbyz)
# target_include_directories(kvblockchain PUBLIC ../../../rocksdb/include)
# target_include_directories(kvblockchain PUBLIC ../Inc)

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
