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
target_compile_definitions(kvblockchain PRIVATE USE_ROCKSDB=1)
endif()
