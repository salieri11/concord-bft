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


# Project Dependencies

## Installing log4cplus

This project usese log4cplus logging framework. We are currently
using version 1.2.1 of this library. Do not use version 2.0 or further
as it changes some interfaces and header files

Follow below steps for installing this library.

1. Download the tarball of this version from log4cplus sourceforge website:

```
wget https://sourceforge.net/projects/log4cplus/files/log4cplus-stable/1.2.1/log4cplus-1.2.1.tar.bz2/download
```

2. Extract the tarball

```
tar xvf log4cplus-1.2.1.tar.bz2
```

3. Move to the extracted directory

```
cd log4cplus-1.2.1
````

4. Configure & Make and install

```
./configure
make
sudo make install
```

This will install all library files and header files into '/usr/local'.

Try compiling and running a simple logger code to make sure everything works.


TODO: implement the equivalent of
P2_Blockchain/Blockchain/basicBlockchain2 and
P2_Blockchain/Blockchan/EtheriumBlockchain here.