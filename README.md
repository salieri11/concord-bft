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

This project uses protocol buffers, so you'll need both the protoc
compile and the C++ headers. On Ubunutu, you can get these via apt:

```
$ sudo apt-get install libprotobuf-dev protobuf-compiler
```

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