# VMware Blockchain Persephone

Persephone is VMW Blockchain's component responsible for creating, managing and 
configuring deployments and upgrades.

Persephone is a Java Spring Boot project.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Persephone components. The
rest of this section explains how to build Persephone natively, which can
be useful for debugging.

### Prerequisites

[Java 11](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

[Protocol Buffers v2.x](https://developers.google.com/protocol-buffers/docs/proto)


### Building all Persephone components

 * Install dependencies and build using maven.

   To build only with unit tests (No Concord instance is needed) :

```
mvn clean package
```


## Components
[Agent](./agent) Service to monitor/upgrade Concord-replicas

[API](./api) Proto files to enable communication with Helen and within Persephone

[Config service](./config-service) Configure components like Concord, Wavefront, Telegraf, logging, etc. 

[IP Allocation service](./ip-allocation-service) Service to handle IPAM blocks.

[Provisioning service](./provisioning-service) Service handling deployments.
