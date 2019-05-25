# Concord source code overview

Instruction on how to build, deploy and test Concord can be found [here](../README.md).

## Source structure

```
.
├── api                      # Gateway for requests coming from Helen or Ethrpc
├── CMakeLists.txt
├── common                   # Shared utility code used by all parts of Concord
    └── CMakeLists.txt
├── config                   # Configuration manager
├── consensus                # Replica & Client interface for the BFT consensus engine
    └── CMakeLists.txt
├── ethereum                 # Interface between Concord and EVMJIT
    └── CMakeLists.txt
├── main.cpp
├── storage                  # Interface to the database / key-value-store to be used by the smart contract engine
    └── CMakeLists.txt
├── time                     # Time Service components
    └── CMakeLists.txt
└── utils                    # Shared utility library used by Concord and its tools and tests
    └── CMakeLists.txt
```

## Namespaces

There are two namespace hierarchies used within the source code:
* `concord::` -> Refers to the source structure above (e.g. `concord::ethereum::EVM`)
* `com::vmware::concord::` -> The protobuf interface defined in [concord.proto](../../communication/src/main/proto/concord.proto) and [concord_storage.proto](../proto/concord_storage.proto)
