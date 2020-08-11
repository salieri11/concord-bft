# Stark

Stark is a test framework for VMW Blockchain's SaaS interface.

### Prerequisites

[Java 11](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

api.yaml file 

#### macOS Setup

Install Java from the link above in the prerequisites, but maven can
be installed via brew.

```
brew install boost aclocal protobuf@2.5 automake autoconf maven
```

Boost and protobuf won't be automatically linked

```
export DYLD_LIBRARY_PATH=/usr/local/Cellar/boost/1.66.0/lib:$DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/usr/local/Cellar/protobuf@2.5/2.5.0/lib:$DYLD_LIBRARY_PATH
```

### Building and Running Stark

```
mvn -Dyaml.file=<path_to_blockchain_dir>/vmwathena_blockchain/helen/src/main/resources/api-doc/api.yaml clean test
```
This file path has to be absolute, relative path does not work.
Note: If you don’t supply yaml file as command line argument, it looks for api.yaml file in the stark project root directory.

