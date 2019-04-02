# Concord source code overview

Instruction on how to build, deploy and test Concord can be found [here](../README.md).

```
.
├── api                      # Gateway for requests coming from Helen or Ethrpc
├── blockchain               # Interface to the database / key-value-store to be used by the smart contract engine
├── CMakeLists.txt
├── common                   # Shared utility code used by all parts of Concord
├── config                   # Configuration manager
├── consensus                # Replica & Client interface between Concord and the BFT engine
    └── kvb                  # Database connection library linked with the BFT engine
        └── CMakeLists.txt
├── ethereum                 # Interface between Concord and EVMJIT
├── main.cpp
└── utils                    # Shared utility library used by Concord and its tools and tests
    └── CMakeLists.txt
```
