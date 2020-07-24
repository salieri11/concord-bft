# VMware Blockchain SaaS API

Castor is VMW Blockchain's on-prem interface. It provides a front-end (not UI) for managing
on-prem blockchain deployments specifically. It is different from Helen in that Castor is
for on-prem (SDDC) only.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Castor. The
rest of this section explains how to build Castor natively, which can
be useful for debugging.

### Prerequisites

[Java 11](https://adoptopenjdk.net/)

[Maven 3.6.3](https://maven.apache.org/)

[Protocol Buffers v2.x](https://developers.google.com/protocol-buffers/docs/proto)

#### macOS Setup

Install Java from the link above in the prerequisites, but maven can
be installed via brew.

```
brew install boost aclocal protobuf@2.5 automake autoconf maven
```

#### Communication

Castor depends on the the communication module, so you should have built
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
java -jar blockchain-castor*.jar
```

### Usage

 * WIP Docker-compose instructions here
