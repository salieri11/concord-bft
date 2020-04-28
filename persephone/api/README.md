# VMware Blockchain Persephone API

## Protobuf API
The API package is responsible for creation of stubs that enable communication within Persephone 
and between Persephone and Helen.

We use Google's [Protocol Buffers](https://developers.google.com/protocol-buffers) protos to define
the objects which are converted to Java stubs using the [Xolstice](https://github.com/xolstice/protobuf-maven-plugin)
Maven plugin.

### Prerequisites

[Java 11](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

[Protocol Buffers v2.x](https://developers.google.com/protocol-buffers/docs/proto)

### Building the package

 * Install dependencies and build using maven.

   To build only with unit tests (No Concord instance is needed) :

```
mvn clean package
```