# VMware Project Athena

Project Athena's goal is to provide an easy-to-deploy blockchain
solution, with stronger guarantees than longest-chain concensus
systems like "proof of work" or "proof of stake", and better
performance than classic Byzantine fault tolerant algorithms like
PBFT.

The project is composed of three components:

 * (SBFT)[vmwathena/sbft] is the consensus engine for Athena

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

4. Edit configure to change "am__api_version" from 1.14 to 1.15, the version that ubuntu 16.04 supports.

5. Configure/make/install

```
./configure
make
sudo make install
```

This will install all library files and header files into
'/usr/local'. (You may need to add `/usr/local/lib` to your
`LD_LIBRARY_PATH` to run Athena.)

### Athena

Once dependencies are installed, run `make`:

```
$ make
```

This should produce an `athena` executable. Run it to start athena:

```
$ athena
VMware Project Athena
Listening on 0.0.0.0:5458
```

With Athena running, you probably want to go set up
[Helen](https://github.com/vmwathena/helen) to talk to it.

## What is here:

 * src/main.cpp: reads program options and starts the application

 * src/athena_acceptor.*: socket listener and acceptor

 * src/athena_connection.*: connection handler (reads and processes requests)

 * src/athena.proto: Google Protocol Buffers messages that Helen uses
   to talk to Athena


## Future Work

TODO: implement the equivalent of
P2_Blockchain/Blockchain/basicBlockchain2 and
P2_Blockchain/Blockchan/EtheriumBlockchain here.
