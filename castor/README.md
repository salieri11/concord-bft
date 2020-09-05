# VMware Blockchain Deployment Product (aka On-prem shell)

Castor is VMW Blockchain's on-prem interface. It provides a front-end (not UI) for managing
on-prem blockchain deployments specifically. It is different from Helen in that Castor is
for on-prem SDDCs only.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Castor. The
rest of this section explains how to build Castor natively, which can
be useful for debugging.

### Prerequisites

[Java 11](https://adoptopenjdk.net/)

[Maven 3.6.3 (minimum)](https://maven.apache.org/)

[Protocol Buffers v3](https://developers.google.com/protocol-buffers/docs/proto)

### Building Castor

  * Build using maven in the /castor directory:
```
mvn clean install
```

### Usage

Users provide configuration information for their blockchains to Castor via _descriptor_ files. 
There are 2 descriptor files:
- infrastructure descriptor
> This file contains information that pertains to the infrastructure: the vCenter URL, credentials, 
> the DAML SDK version to use, the location of the docker registry from where to pull images for the blockchain
> nodes, the zones defined in the SDDC, etc.

- deployment descriptor 
> This file contains blockchain specific information, e.g. the type of blockchain, the number and placement across
> zones of committers and clients, the IP addresses to be used for the committers and clients, etc.

After Castor runs, the results of the provisioning are available in the output directory provided by users when
they launch the Castor docker-compose file. This file contains human-readable data, e.g. the login information
for a provisioned blockchain node, the IP address, client endpoint, etc.  

The output file is named 
```
<consortium-name>_<UTC-datetime>
```

The Castor deployment is made up of 2 docker-compose files:

- docker-compose-castor-prereqs.yml
> This file launches the config service and the provisioning service. It needs to be passed in
> the value of the host IP (the host on which the docker-compose command is run) so that the agents
> can reach into the host for configuration information.

It should be launched so:

```
CONFIG_SERVICE_IP=<host-ip> docker-compose -f docker-compose-castor-prereqs.yml up
```

e.g.
```
CONFIG_SERVICE_IP=10.72.218.2 docker-compose -f docker-compose-castor-prereqs.yml up
```


This will launch the 2 services. They can, and should, stay up for ever.
 
- docker-compose-castor.yml
> This file launches the Castor service. It can be launched for every deployment, and multiple
> instances can be launched concurrently. This will be the case if you wished to deploy multiple blockchains with the
> different configurations. Or concurrently launch the provisioning of multiple instances of the same configuration.

It should be launched so:

**NOTE**: Run this in the same directory as the above command for docker-compose-castor-prereqs.yml

```
CASTOR_DESCRIPTORS_LOC=<descriptor-directory> CASTOR_OUTPUT_DIR=<output-directory> docker-compose -f docker-compose-castor.yml run castor --name <unique-name>
```

e.g.

```
CASTOR_DESCRIPTORS_LOC=/tmp/castor/descriptors CASTOR_OUTPUT_DIR=/tmp/castor/output docker-compose -f docker-compose-castor.yml run castor --name castor222
```

**NOTE** carefully the _run_ command to docker-compose, and the _--name_ option. Every concurrent run needs to 
have a unique name so that docker can launch different instances.
