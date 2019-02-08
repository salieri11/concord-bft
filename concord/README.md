# VMware Blockchain Replica

Concord is the actual blockchain node in this system. It ties the
[Concord-BFT](https://github.com/vmware/concord-bft) consensus system
to the [EVMJit](https://github.com/ethereum/evmjit) execution
environment.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Concord. The
rest of this section explains how to build Concord natively, which can
be useful for debugging. If you run into trouble with the following
steps, compare them to the steps in (./DockerfilePrereqs) and
(./Dockerfile), which are used to build concord for deployment.

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
git checkout 0998bfcb6b00aec85cf8d755d2a70d19ea3051fd
mkdir build/
cd build/
cmake -DALLOC=AUTO -DWSIZE=64 -DRAND=UDEV -DSHLIB=ON -DSTLIB=ON -DSTBIN=OFF -DTIMER=HREAL -DCHECK=on -DVERBS=on -DARITH=x64-asm-254 -DFP_PRIME=254 -DFP_METHD="INTEG;INTEG;INTEG;MONTY;LOWER;SLIDE" -DCOMP="-O3 -funroll-loops -fomit-frame-pointer -finline-small-functions -march=native -mtune=native" -DFP_PMERS=off -DFP_QNRES=on -DFPX_METHD="INTEG;INTEG;LAZYR" -DPP_METHD="LAZYR;OATEP" ..
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

configuring with given flags is important. If log4cplus is build
without `c++11` then concord will give linker errors while building.

This will install all library files and header files into
'/usr/local'. You may need to add `/usr/local/lib` to your
`LD_LIBRARY_PATH` to run Concord. You may also need to export
CPLUS_INCLUDE_PATH variable set to /usr/local/include for the header
files

#### Evmjit

Concord uses the [Evmjit](https://github.com/ethereum/evmjit) VM to
execute Ethereum code. While we're figuring out dependency management,
please clone evmjit to the same directory you cloned concord (i.e. one
directory up from this README file), and build it.

*Important*: Make sure you have LLVM 5.0 installed (as noted in the
*pre-built section above), or your build will take a long* time!

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

Note: On OSX, in addition, you might need to build the standalone
version with an extra step.

```shell
$ make evmjit-standalone
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

Concord uses GoogleTest framework for unit testsing. We also need that
during the build process of concord. please clone google test to the
same directory you cloned concord (i.e. one directory up from this
README file), and build it

```
git clone git@github.com:google/googletest.git
cd googletest
git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
mkdir _build
cd _build
cmake -DCMAKE_CXX_FLAGS="-std=c++11" ..
make

```

Note: the build directory starts with and underscore (`_`) it is
required to use the exact same name

### RocksDB

Concord uses [RocksDB](https://rocksdb.org/) for persistence across
restarts.

First install RocksDB dependencies:

```shell
sudo apt-get install libsnappy-dev zlib1g-dev libbz2-dev liblz4-dev libzstd-dev
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

This should produce a `concord` executable.

Run the executable to start concord:

```shell
concord/build$ ./src/concord -c test/resources/concord1.config
2019-01-29T22:52:38.742 [140149696604672] INFO  concord.main %% VMware Project concord starting [/home/bfink/vmwathena/blockchain/concord/src/main.cpp:353]
2019-01-29T22:52:38.742 [140149696604672] INFO  concord.main %% Reading genesis block from ./test/resources/genesis.json [/home/bfink/vmwathena/blockchain/concord/src/main.cpp:204]
...
2019-01-29T22:52:38.798 [140149696604672] INFO  concord.main %% Listening on 0.0.0.0:5459 [/home/bfink/vmwathena/blockchain/concord/src/main.cpp:299]
...
```

You will need to start an additional two replicas in other terminals
to actually make progress in handling commands:

```shell
concord/build$ ./src/concord -c test/resources/concord2.config
```

```shell
concord/build$ ./src/concord -c test/resources/concord3.config
```

The test config specifies four replicas total, so also start the final
one, if you want a full healthy network running:

```shell
concord/build$ ./src/concord -c test/resources/concord4.config
```

With Concord running, you probably want to set up [Helen](../helen) or
[EthRPC](../ethrpc) to talk to it.

## What is here:

 * src/main.cpp: reads program options and starts the application

 * src/concord_acceptor.*: socket listener and acceptor

 * src/concord_connection.*: connection handler (reads and processes requests)

 * src/concord_evm.*: wrapper around Evmjit

 * ../communication/src/main/proto/concord.proto: Google Protocol
   Buffers messages that Helen uses to talk to Concord

 * tools/conc_sendtx.cpp: a utility that allows you to send
   transactions to concord without needing to build and start helen

 * tools/conc_gettxrcpt.cpp: a utility that allows you to fetch a
   transaction receipt without needing to build and start helen
