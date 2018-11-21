# VMware Project Concord

Project Concord's goal is to provide an easy-to-deploy blockchain
solution, with stronger guarantees than longest-chain consensus
systems like "proof of work" or "proof of stake", and better
performance than classic Byzantine fault tolerant algorithms like
PBFT.

The project is composed of three components:

 * [Concord-BFT](https://github.com/vmware/concord-bft) is the consensus engine for
   concord.

 * concord (this subdirectory) provides a key-value
   abstraction atop SBFT, and an Etherium VM compatibility layer on
   top of that KV storage

 * [helen](../helen) is the home of Concord's UI and external API

## Building

### Dependencies

#### Pre-built libraries and tools

You will need cmake, clang, and g++, gmp, GNU Parallel, autoconf, automake, and LLVM 5.0:

```
sudo apt-get install cmake clang g++ parallel autoconf automake llvm-5.0 llvm-5.0-dev
```

#### Relic

Then clone and build Relic:

```
cd
git clone https://github.com/relic-toolkit/relic
cd relic/
mkdir build/
cd build/
cmake -DALLOC=AUTO -DWORD=64 -DRAND=UDEV -DSHLIB=ON -DSTLIB=ON -DSTBIN=OFF -DTIMER=HREAL -DCHECK=on -DVERBS=on -DARITH=x64-asm-254 -DFP_PRIME=254 -DFP_METHD="INTEG;INTEG;INTEG;MONTY;LOWER;SLIDE" -DCOMP="-O3 -funroll-loops -fomit-frame-pointer -finline-small-functions -march=native -mtune=native" -DFP_PMERS=off -DFP_QNRES=on -DFPX_METHD="INTEG;INTEG;LAZYR" -DPP_METHD="LAZYR;OATEP" ..
make
sudo make install
```

#### Boost

You will need Boost installed. Version 1.64.0 has been tested:

```
wget https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.tar.gz
tar -xzf boost_1_64_0.tar.gz
cd boost_1_64_0
./bootstrap.sh --with-libraries=system,filesystem,program_options,thread --prefix=/usr
./b2
sudo ./b2 install
```

#### Protocol Buffers

This project uses protocol buffers, so you'll need both the protoc
compile and the C++ headers. On Ubuntu, you can get these via apt:

```
$ sudo apt-get install libprotobuf-dev protobuf-compiler
```

#### Log4CPlus

This project uses log4cplus logging framework. We are currently
using version 1.2.1 of this library. Do not use version 2.0 or further
as it changes some interfaces and header files

Follow below steps for installing this library.

1. Clone the repository.

```
git clone https://github.com/log4cplus/log4cplus.git
```

2. Move to the extracted directory and checkout the appropriate branch.

```
cd log4cplus
git checkout REL_1_2_1
````

3. Edit `configure` to change "am__api_version" from 1.14 to 1.15, the
version that ubuntu 16.04 supports.

4. Configure/make/install

```
./configure CXXFLAGS="--std=c++11"
make
sudo make install
```
configuring with given flags is important. If log4cplus is build without `c++11` then concord will
give linker errors while building.

This will install all library files and header files into
'/usr/local'. (You may need to add `/usr/local/lib` to your
`LD_LIBRARY_PATH` to run Concord.)
You may also need to export CPLUS_INCLUDE_PATH variable set to /usr/local/include for the header files

#### Evmjit

concord uses the [Evmjit](https://github.com/ethereum/evmjit) VM to
execute Ethereum code. While we're figuring out dependency management,
please clone evmjit to the same directory you cloned concord (i.e. one
directory up from this README file), and build it.

*Important*: Make sure you have LLVM 5.0 installed (as noted in the pre-built section above), or your build will take a
*long* time!

Clone and build Evmjit (note the path to LLVM cmake files):

```shell
concord$ cd ..
$ git clone git@github.com:ethereum/evmjit.git
$ cd evmjit
### their interface is evolving rapidly, so use this specific hash
$ git checkout 4e9f3d76292c7de0c6613427761f843b1719f614
$ mkdir build
$ cd build
$ cmake -DLLVM_DIR=/usr/lib/llvm-5.0/lib/cmake/llvm ..
$ cmake --build . --config RelWithDebInfo
```

#### Cryptopp

Concord uses [Crypto++](https://github.com/weidai11/cryptopp.git) to
compute SHA3/Keccak-256 hashes. You will need to download and build
it:

```
git clone https://github.com/weidai11/cryptopp.git
cd cryptopp/
git checkout CRYPTOPP_5_6_5;
mkdir build/
cd build/
cmake ..
make
sudo make install
```

#### Secp256k1

Concord uses [secp256k1](https://github.com/bitcoin-core/secp256k1) to
recover sender addresses from transaction signatures. You will need to
download and build it:

```
git clone git@github.com:bitcoin-core/secp256k1
cd secp256k1
# just in case interfaces change, documenting tested commit:
git checkout 1e6f1f5ad5e7f1e3ef79313ec02023902bf8175c
./autogen.sh
./configure --enable-module-recovery
make
./tests
sudo make install
```

#### Google Test
Concord uses GoogleTest framework for unit testsing. We also need that during the build process of concord. please clone google test to the same directory you cloned concord (i.e. one directory up from this README file), and build it
```
git clone git@github.com:google/googletest.git
cd googletest
git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
mkdir _build
cd _build
cmake -DCMAKE_CXX_FLAGS="-std=c++11" ..
make

```
Note: the build directory starts with and underscore (_) it is required to use the exact same name

### Persistence

Concord can also be run using persistent storage for the blockchain data.
The persistent storage used currently is [RocksDB](https://rocksdb.org/).

Build and install RocksDB dependencies:

```shell
sudo apt-get install libsnappy-dev
sudo apt-get install zlib1g-dev
sudo apt-get install libbz2-dev
sudo apt-get install liblz4-dev
sudo apt-get install libzstd-dev
```

Build and install RocksDB:

```shell
cd
wget https://github.com/facebook/rocksdb/archive/v5.7.3.tar.gz
tar -xzf v5.7.3.tar.gz
cd rocksdb-5.7.3
make shared_lib
sudo make install-shared
```

Tell CMakeLists that you want to use RocksDB:

```shell
concord$ emacs CMakeLists.txt &
```

Set Use_RocksDB to True

Also, provide a path to the location where you want the database files to be stored:

```shell
concord$ emacs resources/concord.config &
```

If not present, add a key `blockchain_db_impl` and set its value to `rocksdb`.
If not present, add a key `blockchain_db_path` and set its value to the path of the
location where storage is desired.

### Concord

Once dependencies are installed, make sure you have initialized the
submodules. The first time you build, this is done by:

```shell
concord$ git submodule init
concord$ git submodule update --recursive
```

When subsequent updates are needed, omit the `init` command, and just
run the `update` command. Tip: you can change your local URL for
any submodule by issuing the following command:

```shell
concord$ git config submodule.submodules/<submodule name>.url <alternate url>
```

Once the submodule is updated, build concord:

```shell
concord$ mkdir build
concord$ cd build
concord/build$ cmake ..
concord/build$ make
```

This should produce an `concord` executable.

At this stage, you may provide the path to a genesis file if you have one.

```shell
concord$ emacs resources/concord.config &
```

Modify the value of the `genesis_block` key with the absolute path to your genesis file.

Run the executable to start concord:

```shell
concord/build$ ./src/concord -c resources/concord1.config
2018-03-28T17:35:10.712 [140229951600448] INFO  concord.main %% VMware Project Concord starting [/home/bfink/vmwathena/concord/src/main.cpp:84]
2018-03-28T17:35:10.713 [140229951600448] INFO  concord.evm %% EVM started [/home/bfink/vmwathena/concord/src/concord_evm.cpp:43]
2018-03-28T17:35:10.713 [140229951600448] INFO  concord.main %% Listening on 0.0.0.0:5458 [/home/bfink/vmwathena/concord/src/main.cpp:54]
```

You will need to start an additional two replicas in other terminals
to actually make progress in handling commands:

```shell
concord/build$ ./src/concord -c resources/concord2.config
```

```shell
concord/build$ ./src/concord -c resources/concord3.config
```

With Concord running, you probably want to go set up
[Helen](https://github.com/vmwathena/helen) to talk to it.

## What is here:

 * src/main.cpp: reads program options and starts the application

 * src/concord_acceptor.*: socket listener and acceptor

 * src/concord_connection.*: connection handler (reads and processes requests)

 * src/concord_evm.*: wrapper around Evmjit

 * proto/concord.proto: Google Protocol Buffers messages that Helen uses
   to talk to Concord

 * tools/ath_sendtx.cpp: a utility that allows you to send
   transactions to concord without needing to build and start helen

 * tools/ath_gettxrcpt.cpp: a utility that allows you to fetch a
   transaction receipt without needing to build and start helen
