# VMware Project Athena

Project Athena's goal is to provide an easy-to-deploy blockchain
solution, with stronger guarantees than longest-chain concensus
systems like "proof of work" or "proof of stake", and better
performance than classic Byzantine fault tolerant algorithms like
PBFT.

The project is composed of three components:

 * ~(SBFT)[vmwathena/sbft]~ (temporarily
   (P2_Blockchain)[vmwathena/P2_Blockchain]) is the consensus engine
   for Athena.

 * (athena)[vmwathena/athena] (this repo) provides a key-value
   abstraction atop SBFT, and an Etherium VM compatibility layer on
   top of that KV storage

 * (helen)[vmathena/helen] is the home of Athena's UI and external API

## Building

### Dependencies

#### Boost

You will need Boost installed. Version 1.64.0 has been tested (note
the addition of `program_options` if you had built Boost for the
`P2_Blockchain` project already:

```
wget https://dl.bintray.com/boostorg/release/1.64.0/source/boost_1_64_0.tar.gz
tar -xzf boost_1_64_0.tar.gz
cd boost_1_64_0
./bootstrap.sh --with-libraries=system,filesystem,program_options --prefix=/usr
./b2
sudo ./b2 install
```

#### Protocol Buffers

This project uses protocol buffers, so you'll need both the protoc
compile and the C++ headers. On Ubunutu, you can get these via apt:

```
$ sudo apt-get install libprotobuf-dev protobuf-compiler
```

#### Log4CPlus

This project usese log4cplus logging framework. We are currently
using version 1.2.1 of this library. Do not use version 2.0 or further
as it changes some interfaces and header files

Follow below steps for installing this library.

1. Install prerequisites.
```
sudo apt-get install autoconf automake
```

2. Clone the repository.

```
git clone https://github.com/log4cplus/log4cplus.git
```

3. Move to the extracted directory and checkout the appropriate branch.

```
cd log4cplus
git checkout REL_1_2_1
````

4. Edit `configure` to change "am__api_version" from 1.14 to 1.15, the
version that ubuntu 16.04 supports.

5. Configure/make/install

```
./configure CXXFLAGS="--std=c++11"
make
sudo make install
```
configuring with given flags is important. If log4cplus is build without `c++11` then athena will
give linker errors while building.

This will install all library files and header files into
'/usr/local'. (You may need to add `/usr/local/lib` to your
`LD_LIBRARY_PATH` to run Athena.)
You may also need to export CPLUS_INCLUDE_PATH variable set to /usr/local/include for the header files

#### Evmjit

Athena uses the [Evmjit](https://github.com/ethereum/evmjit) VM to
execute Ethereum code. While we're figuring out dependency management,
please clone evmjit to the same directory you cloned athena (i.e. one
directory up from this README file), and build it:

```shell
athena$ cd ..
$ git clone git@github.com:ethereum/evmjit.git
$ cd evmjit
### their interface is evolving rapidly, so use this specific hash
$ git checkout 4e9f3d76292c7de0c6613427761f843b1719f614
$ mkdir build
$ cd build
$ cmake ..
$ cmake --build . --config RelWithDebInfo
```

Warning: this will download and compile LLVM, which takes about an
hour.

#### Cryptopp

Athena uses [Crypto++](https://github.com/weidai11/cryptopp.git) to
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

#### Google Test
Athena uses GoogleTest framework for unit testsing. We also need that during the build process of athena. please clone google test to the same directory you cloned athena (i.e. one directory up from this README file), and build it
```
git clone git@github.com:google/googletest.git
cd googletest
mkdir _build
cd _build
cmake ..
make

```
Note: the build directory starts with and underscore (_) it is required to use the exact same name

### Athena

Once dependencies are installed, make sure you have initialized the
P2_blockchain submodule. The first time you build, this is done by:

```shell
athena$ git submodule init
athena$ git submodule update --recursive
```

When subsequent updates are needed, omit the `init` command, and just
run the `update` command. Tip: you can change your local URL for
P2_Blockchain by issuing the following command:

```shell
athena$ git config submodule.submodules/P2_Blockchain.url <alternate url>
```

Once the submodule is updated, build athena:

```shell
athena$ mkdir build
athena$ cd build
athena/build$ cmake ..
athena/build$ make
```

This should produce an `athena` executable. 

At this stage, you may provide the path to a genesis file if you have one.

```shell
athena$ emacs resources/athena.config &
```

Modify the value of the `genesis_block` key with the absolute path to your genesis file.

Run the executable to start athena:

```shell
athena/build$ ./src/athena -c resources/athena1.config
2018-03-28T17:35:10.712 [140229951600448] INFO  athena.main %% VMware Project Athena starting [/home/bfink/vmwathena/athena/src/main.cpp:84]
2018-03-28T17:35:10.713 [140229951600448] INFO  athena.evm %% EVM started [/home/bfink/vmwathena/athena/src/athena_evm.cpp:43]
2018-03-28T17:35:10.713 [140229951600448] INFO  athena.main %% Listening on 0.0.0.0:5458 [/home/bfink/vmwathena/athena/src/main.cpp:54]
```

You will need to start an additional two replicas in other terminals
to actually make progress in handling commands:

```shell
athena/build$ ./src/athena -c resources/athena2.config
```

```shell
athena/build$ ./src/athena -c resources/athena3.config
```

With Athena running, you probably want to go set up
[Helen](https://github.com/vmwathena/helen) to talk to it.

### Persistence

Athena can also be run using persistent storage for the blockchain data. 
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
make static_lib
sudo make install
```

Tell CMakeLists that you want to use RocksDB:

```shell
athena$ emacs CMakeLists.txt &
```

Set Use_RocksDB to True

Also, provide a path to the location where you want the database files to be stored:

```shell
athena$ emacs resources/athena.config &
```

If not present, add a key `blockchain_db_impl` and set its value to `rocksdb`.
If not present, add a key `blockchain_db_path` and set its value to the path of the
location where storage is desired.

## What is here:

 * src/main.cpp: reads program options and starts the application

 * src/athena_acceptor.*: socket listener and acceptor

 * src/athena_connection.*: connection handler (reads and processes requests)

 * src/athena_evm.*: wrapper around Evmjit

 * proto/athena.proto: Google Protocol Buffers messages that Helen uses
   to talk to Athena

 * tools/ath_sendtx.cpp: a utility that allows you to send
   transactions to athena without needing to build and start helen

 * tools/ath_gettxrcpt.cpp: a utility that allows you to fetch a
   transaction receipt without needing to build and start helen