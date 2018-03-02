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

This will install all library files and header files into '/usr/local'.

Try compiling and running a simple logger code to make sure everything works.

## Installing ninja (needed for jsoncpp, which is only needed for testing)
1. Download it.
```
wget https://github.com/ninja-build/ninja/archive/v1.8.2.tar.gz
```

2. Extract it.
```
tar -xf v1.8.2.tar.gz
```

3. Build/configure/install.
```
cd ninja-1.8.2/
./configure.py --bootstrap
sudo cp ninja /usr/sbin/ninja
```

## Installing meson (needed for jsoncpp, which is only needed for testing)

1. Download it.
```
wget https://github.com/mesonbuild/meson/archive/0.44.1.tar.gz
```

2. Extract it.
```
tar -xf 0.44.1.tar.gz
```

3. Build it.
```
cd meson-0.44.1
sudo python3 setup.py install
```

## Installing jsoncpp (which is only needed for testing)

1. Download it.
```
wget https://github.com/open-source-parsers/jsoncpp/archive/1.8.4.tar.gz
```

2. Extract it.
```
tar -xf 1.8.4.tar.gz
```

3. Build/test/install.
```
cd jsoncpp-1.8.4
mkdir build-shared
meson --buildtype release --default-library shared . build-shared
ninja -v -C build-shared test
cd build-shared
sudo ninja install
```

TODO: implement the equivalent of
P2_Blockchain/Blockchain/basicBlockchain2 and
P2_Blockchain/Blockchan/EtheriumBlockchain here.
