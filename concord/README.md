# VMware Blockchain Replica

Concord is the actual blockchain node in this system. It ties the
[Concord-BFT](https://github.com/vmware/concord-bft) consensus system
to the [EVMJit](https://github.com/ethereum/evmjit) execution
environment.

## Building

Please see [../README.md](README.md) in the parent directory for how to build
docker images, which is the normal way to build Concord. The rest of this
section explains how to build Concord natively, which can be useful for
debugging. These steps primarily target building Concord on Ubuntu 18.04. If you
run into trouble with the following steps, compare them to the steps in
(./DockerfilePrereqs) and (./Dockerfile), which are used to build concord for
deployment.

### Dependencies

#### Pre-built libraries and tools

You will need cmake, clang, and g++, gmp, GNU Parallel, autoconf, automake, LLVM
5.0, GMP 3, GNU Libtool, several Boost libraries (at the time of this writing,
we use program-options, system, and thread), and yaml-cpp:

```
sudo apt-get install cmake clang-7 clang-format-7 g++ parallel autoconf \
  automake llvm-5.0 llvm-5.0-dev libgmp3-dev libtool libboost1.65-dev \
  libboost-program-options1.65-dev libboost-program-options1.65.1 \
  libboost-system1.65-dev libboost-system1.65.1 libboost-thread1.65-dev \
  libboost-filesystem1.65-dev libboost-thread1.65.1 libyaml-cpp0.5v5 libyaml-cpp-dev
```

#### Relic

Then clone and build [Relic](https://github.com/relic-toolkit/relic):

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

#### Protocol Buffers

This project uses protocol buffers, so you'll need both the protoc
compile and the C++ headers. On Ubuntu, you can get these via apt:

```
$ sudo apt-get install libprotobuf-dev protobuf-compiler
```

#### Log4CPlus

This project uses [log4cplus](https://github.com/log4cplus/log4cplus) logging
framework. We are currently using version 1.2.1 of this library. Do not use
version 2.0 or further as it changes some interfaces and header files

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
version that Ubuntu 18.04 supports.

4. Configure/make/install

```
./configure CXXFLAGS="--std=c++11" --enable-static
make
sudo make install
```

configuring with given flags is important. If log4cplus is build without `c++11`
then concord will give linker errors while building. If log4cplus is built
without `--enable-static`, CMake may complain it cannot find a log4cplus static
library (which is needed for building `conc_genconfig`).

This will install all library files and header files into
'/usr/local'. You may need to add `/usr/local/lib` to your
`LD_LIBRARY_PATH` to run Concord. You may also need to export
CPLUS_INCLUDE_PATH variable set to /usr/local/include for the header
files

#### Evmjit

Concord uses the [Evmjit](https://github.com/ethereum/evmjit) VM to
execute Ethereum code. While we're figuring out dependency management,
please clone evmjit to the same directory you cloned blockchain (i.e. two
directories up from this README file), and build it.

*Important*: Make sure you have LLVM 5.0 installed (as noted in the
*pre-built section above), or your build will take a long* time!

Clone and build Evmjit (note the path to LLVM cmake files):

```shell
blockchain/concord$ cd ../..
$ git clone git@github.com:ethereum/evmjit.git
$ cd evmjit
### their interface is evolving rapidly, so use this specific hash
$ git checkout 4e9f3d76292c7de0c6613427761f843b1719f614
$ mkdir build
$ cd build
$ cmake -DLLVM_DIR=/usr/lib/llvm-5.0/lib/cmake/llvm ..
$ cmake --build . --config RelWithDebInfo
```

Note: On OSX, in addition, you might need to build the standalone version with
an extra step.

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

Concord uses [GoogleTest](https://github.com/google/googletest) framework for
unit testsing. We also need that during the build process of concord. please
clone google test to the same directory you cloned blockchain (i.e. two
directories up from this README file), and build it:

```
git clone git@github.com:google/googletest.git
cd googletest
git checkout e93da23920e5b6887d6a6a291c3a59f83f5b579e
mkdir _build
cd _build
cmake -DCMAKE_CXX_FLAGS="-std=c++11" ..
make
sudo make install
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

### OpenSSL (if used with TLS module)

Concord uses [OpenSSL](https://github.com/openssl/openssl) for TLS communication. You will need to install OpenSSL version 1.1.1a:

```shell
   git clone https://github.com/openssl/openssl.git
   cd openssl
   git checkout OpenSSL_1_1_1a
   ./config
   make
   make test
   sudo make install
```
Then run:

```shell
   openssl version
```
If you get an error then run:

```shell
   export LD_LIBRARY_PATH=/usr/local/lib
```

You should get 1_1_1a as your version.

### gRPC

Concord uses [gRPC](https://github.com/grpc/grpc) for DAML and HLF api server. You will need to install grpc version v1.17.x:

```shell
   git clone https://github.com/grpc/grpc
   cd grpc
   git checkout v1.17.x
   git submodule update --init
   cd third_party/protobuf
   git checkout 3.6.x
   ./autogen.sh
   ./configure --prefix=/opt/protobuf
   make -j4
   sudo make install
   cd ../..
   make -j4 PROTOC=/opt/protobuf/bin/protoc
   sudo make prefix=/opt/grpc install
```

### Thrift

Thrift is a dependency of Concord's tracing library, Jaeger.

```shell
wget http://apache.mirrors.hoobly.com/thrift/0.11.0/thrift-0.11.0.tar.gz
tar xzf thrift-0.11.0.tar.gz
cd thrift-0.11.0
./configure
make -j4
sudo make install
```

### OpenTracing

OpenTracing is a dependency of Concord's tracing library, Jaeger.

```shell
git clone https://github.com/opentracing/opentracing-cpp
cd opentracing-cpp
git checkout v1.5.0
mkdir build
cd build
cmake ..
make -j4
sudo make install
```

### JSON

JSON is a dependency of Concord's tracing library, Jaeger.

```shell
git clone https://github.com/nlohmann/json
cd json
git checkout v3.7.3
mkdir build
cd build
cmake ..
make -j4
sudo make install
```

### Jaeger

Jaeger is the tracing library that Concord uses. Note that these instructions are different from those in the Jaeger readme. These build Jaeger without using the Hunter dependency manager.

```shell
git clone https://github.com/jaegertracing/jaeger-client-cpp
cd jaeger-client-cpp
git checkout v0.5.0
mkdir build
cd build
cmake -DHUNTER_ENABLED=NO -DBUILD_TESTING=NO -DBUILD_SHARED_LIBS=NO -DJAEGERTRACING_BUILD_EXAMPLES=NO ..
make -j4
sudo make install
```

Two additional small steps need to be executed, to allow Concord's
build to find these dependencies.

First, copy the Findthrift CMake script:

```
sudo cp ../cmake/Findthrift.cmake /usr/share/cmake-3.10/Modules/
```

Second, modify the jaegertracing CMake script to prevent use of a non-existent BoostConfig.cmake:

```
sudo sed -i '/boost_components/d' /usr/local/lib/cmake/jaegertracing/jaegertracingConfig.cmake
```

### Concord

At build-time, concord takes advantage of clang-format (v7) to check code
formatting. Make sure you have it installed as well.

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

If the build fails because of formatting issues then you can run the following
command for auto-correction:

```shell
concord/build$ make format
```

The build includes several unit-testing programs. You can run them all
using "make test". Test logs are stored in the `Testing/Temporary/`
directory inside the build directory. Example run:

```shell
Running tests...
Test project /home/bfink/vmwathena/blockchain-alt/concord/build
    Start 1: UtilsTests
1/5 Test #1: UtilsTests .......................   Passed    0.01 sec
    Start 2: SignTests
2/5 Test #2: SignTests ........................   Passed    0.05 sec
    Start 3: SliverTests
3/5 Test #3: SliverTests ......................   Passed    0.00 sec
    Start 4: ConfigTests
4/5 Test #4: ConfigTests ......................   Passed    0.01 sec
    Start 5: MultiIOTests
5/5 Test #5: MultiIOTests .....................   Passed    0.03 sec

100% tests passed, 0 tests failed out of 5

Total Test time (real) =   0.11 sec
```

On a successful build you have a `concord` executable.

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

### Concord Configuration

Concord uses YAML configuration files. A configuration generation utility is
provided to generate these configuration files for a given Concord cluster. In a
completed build, the utility is `conc_genconfig` in the `tools` directory. Note
`conc_genconfig` is statically linked in order to facilitate running it on
machines that are not themsleves Concord nodes during automated Concord
deployment workflows without requiring the installation of Concord's runtime
dependencies on the non-Concord-node machines involved in deployment. Note,
however, that `conc_genconfig` does still have some runtime dependencies on
common shared system libraries (for example, `libpthread`). At the time of this
writing `conc_genconfig` is linked with the objective of being runnable without
installing any dependencies that do not come by default on Ubuntu 18.04.

The utility accepts input providing the size of the cluster, networking
information, and any non-default values you would like to elect for optional
parameters. For example, the following command can be used to generate a
configuration equivalent to the current 4-node test configuration (with a fresh
set of cryptographic keys generated):

```shell
concord/build$ ./tools/conc_genconfig --configuration-input \
               test/resources/config_input/nativeConfigurationInput.yaml
```

This command will output four configuration files to the path it is run from
named `concord<i>.config` for _i_ in the range (1, 4), inclusive. The base
filename (in this case "`concord`") can be specified with the `--output-name`
option to `conc_genconfig`.

An example configuration input files is provided, specifically
`concord/test/resources/config_input/nativeConfigurationInput.yaml`, which
generates a 4-node testing configuration to be run natively like the example
configuration files in `test/resources`. Furthermore, the configuration input
file used to generate configurations for the 4-node Docker cluster used in
testing can be found at `docker/config-public/dockerConfigurationInput.yaml`.

The basic form of both the configuration input and Concord configuration files
is a YAML associative array assigning values to configuration parameters
(`<CONFIGURATION_PARAMETER>: <VALUE>`). For example, at the time of this
writing, the required parameters to size a 4-node Concord cluster can be
specified with:

```
f_val: 1
c_val: 0
client_proxies_per_replica: 4
```

Although some parameters appearing in the configuration files and configuration
input are global in scope, it should be noted that many configuration parameters
assign values for specific entities within a Concord cluster. At the time of
this writing, there are parameters that specify values for individual Concord
nodes, SBFT replicas, and SBFT client proxies (SBFT replicas and client proxies
are entities created and used by Concord-BFT, the SBFT consensus implementation
currently used by Concord; at the time of this writing, each Concord node
contains 1 SBFT replica and 4 SBFT client proxies). These entity-specific
parameters are assigned in the configuration files by assignment of lists to
keys with names representing the type of entity. Each entry in the list
represents one instance of that entity. For example, the network configuration
for a 4-node native Concord test cluster is given in
`nativeConfigurationInput.yaml` with:

```
node:
  - service_host: 0.0.0.0
    service_port: 5458
    replica:
      - replica_host: 127.0.0.1
        replica_port: 3501
    client_proxy:
      - client_host: 127.0.0.1
        client_port: 3505
      - client_host: 127.0.0.1
        client_port: 3506
      - client_host: 127.0.0.1
        client_port: 3507
      - client_host: 127.0.0.1
        client_port: 3508
  - ... # 3 More entries in the node list structurally equivalent to the first,
        # but with different values assigned; omitted in this example for
        # concision; see `nativeConfigurationInput.yaml` for the full list.
```

Furthermore, a shorthand means of selecting a value for a parameter for all
instances in one of these entity lists is provided; specifically, an associative
array assigned to the key `<ENTITY_SCOPE_NAME>__TEMPLATE` in the same
associative array as the key `<ENTITY_SCOPE_NAME>` indicates that the
assignments in the associative array assigned to `<ENTITY_SCOPE_NAME>__TEMPLATE`
should be included in each entry in the list assigned to `<ENTITY_SCOPE_NAME>`.
For example, `nativeConfigurationInput.yaml` includes the lines:

```
node__TEMPLATE:
  genesis_block: ./test/resources/genesis.json
```

These lines are equivalent to adding the assignment `genesis_block:
./test/resources/genesis.json` to each list entry in the `node` list. If a node
configuration file or the configuration input file contains assignments to the
same key for both one of these template shorthands and for entr(y/ies) in the
main list, the entry-specific values will take precedence. For example, if the
configuration input contained these lines:

```
node__TEMPLATE:
  service_port: 5458
node:
  - ...
    service_port: 6000
  ...
```

Then the first Concord node would have `service_port` 6000, but the remaining
nodes would have port 5458 (unless they themselves also have their own
`service_port` assignments in the `node` list).

Internally, the contents of the ConcordConfiguration are defined and managed
with a class, `ConcordConfiguration`, defined in
`concord/src/config/configuration_manager.hpp`. A function
`specifyConfiguration` (declared in
`concord/src/config/configuration_manager.hpp` and implemented in
`concord/src/configuration_manager.cpp`) is used to fill out a
`ConcordConfiguration` and is intended as the primary source of truth on what
the current configuration looks like; it is a good place to start looking if you
need to add a new configuration parameter to Concord.

#### Concord Configuration Generation with Docker

The Concord configuration generation utility is actually included in the image
that can be built with the `Dockerfile` in this directory. You can run
configuration generation in that image with `docker run` by mounting an
appropriate configuration volume and giving the configuration generation
command, for example, to run configuration generation for the 4-node Docker
test cluster we currently use, you can run this command from the
`vmwathena_blockchain/docker` directory (assuming your Concord image is tagged
`concord-core:latest`):

```
vmwathena_blockchain$ docker run \
    --mount type=bind,source=$PWD/config-public,destination=/concord/config \
    concord-core:latest /concord/conc_genconfig \
    --configuration-input /concord/config/dockerConfigurationInput.yaml \
    --output-name /concord/config/concord
```

In the case of the Docker test cluster this command generates configuration for,
we have also provided a script to move the generated configuration files from
`config-public` to the appropriate volumes for each Concord node. Again from
`vmwathena_blockchain/docker`, this script can be run with:

```
vmwathena_blockchain/docker$ ./config-public/distribute-configuration-files.sh
```

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

 * tools/conc_genconfig.cpp: Configuration generation utility which, given input
   containing cluster size and network configuraiton parameters (plus any
   non-default values you want to set for optional configuration parameters),
   outputs a configuration file for each node in a Concord cluster (this utility
   also handles generation of the cryptographic keys required for each node's
   configuration file).

## Time Service

A new "time service" is under development in concord. Its code is
primarily in the src/time/ directory. Details about the design of this
service are here: https://confluence.eng.vmware.com/display/BLOC/Time+Service.

The service is disabled by default. To enable it, set the config parameter
`FEATURE_time_service` to `true` in all of your node config files. Defaults of
`false` and `concord[1..4]` have been added to the example config input files in
`test/resources/config_input/` to help with this process.

The time service optionally supports validation on each node of time updates
from other nodes via one of a couple methods. The time verification can be
selected with the `time_verification` top-level configuration parameter.
Currently, options for this parameter include `rsa-time-signing`,
`bft-client-proxy-id`, and `none` (`rsa-time-signing` will be used by default if
no option is explicitly elected in the configuration). If `rsa-time-signing` is
selected, cryptographic signatures will be appended to time updates published by
any time source node using a private key belonging to that node, and other
nodes will validate these signatures with the corresponding public key to
confirm the updates' legitimacy (a key already used by Concord purposes will be
re-used, so it is not necessary to add any additional keys to use
`rsa-time-signing`). If `bft-client-proxy-id` is selected, time updates will be
validated based on the ID of the Concord-BFT client proxy through which the
updates are published through the consensus layer. This validtion mechanism
works because the consensus layer should guarantee it is intractable to
impersonate a specific client proxy without knowledge of a private key belonging
to that client proxy. If `none` is selected, no verification scheme will be
used by the "time service"; however, using `none` is not recommended in
production environments as it can allow a Byzantine-faulty node to move time
forward against the judgement of the other nodes.

Time sources in the time service can publish time updates to the rest of the
nodes either by appending the time updates to normal transactions or by sending
time updates as their own messages. A time source may publish these updates as
their own messages periodically to ensure its time published to the system does
not grow stale during periods when the time source does not have other
transactions coming in that it can piggyback the time updates off of. The period
which a time source will wait before proactively sending one of these updates is
configurable with the per-node `time_pusher_period_ms` parameter in Concord's
configuration (denominated in milliseconds). Note providing a non-positive value
for this period will cause the time source to publish no proactive updates and
only publish updates when transactions are available to append the updates to.
Concord supports adjustment of this period at runtime. A utility,
`conc_reconfig`, is implemented in the `tools` subdirectory which does this. To
change the update period for a running Concord node run `conc_reconfig` with the
arguments `--time_pusher_period_ms <NEW_TIME_PERIOD>` on the same host as the
time source to be adjusted is running. Note setting the period to be
non-positive in this way will disable no-load time updates and setting the
period to be positive will enable them.

Note that the time service's current implementation relies on the assumption
that the `time_source_id` for any particular time source will not differ in its
in-memory representation at a byte level on any pair of replicas or time
sources; this effectively requires that the time source IDs are stored in the
memory of each node with the same or at least compatible character encodings.
