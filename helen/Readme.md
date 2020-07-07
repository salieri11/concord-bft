# VMware Blockchain SaaS API

Helen is VMW Blockchain's SaaS interface. It exposes APIs for managing
blockchain deployments.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Helen. The
rest of this section explains how to build Helen natively, which can
be useful for debugging.

### Prerequisites

[Java 11](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

[Solidity compiler](https://solidity.readthedocs.io/en/v0.5.2/installing-solidity.html)
Install solidity compiler (`solc`) with version >= 0.5.2

[Protocol Buffers v2.x](https://developers.google.com/protocol-buffers/docs/proto)


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

#### Communication

Helen depends on the the communication module, so you should have built
(../communication) before this.

### Building and Running Helen

 * [Run Concord](../concord).

 * Install dependencies and build using maven.

   To build only with unit tests (No Concord instance is needed) :

```
mvn clean package
```

 * Run the server

```
java -jar blockchain-helen*.jar
```

### API

 * `/api/` - Used to return a list of all other APIs serviced

 * `/api/concord/*` - API endpoints (see
   [swagger](https://github.com/vmwathena/helen/blob/master/webapp/src/static/swagger/swagger.json))

 * `/*` - Used to serve content from src/main/resources/static/index.html
