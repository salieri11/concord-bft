# VMware Blockchain SaaS API

Castor is VMW Blockchain's on-prem interface. It provides a front-end (not UI) for managing
on-prem blockchain deployments specifically. It is different from Helen in that Castor is
for on-prem (SDDC) only.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images, which is the normal way to build Castor. The
rest of this section explains how to build Castor natively, which can
be useful for debugging.

### Building and Running Castor

 * Build using maven.

```
mvn clean package
```

 * Modify these properties in /docker/docker-compose-castor.yml file, to point to the descriptor files:
 
```
     environment:
       - "castor.infrastructure.descriptor.location=/descriptors/test01_infrastructure_descriptor.json"
       - "castor.deployment.descriptor.location=/descriptors/test01_deployment_descriptor.json"
       - "castor.output.directory.location=/output"
```
  
 * Run the docker-compose file from inside the /docker directory. You will need to pass in 
 - (1) host directories for the input and output, and 
 - (2) The IP address of the host on which the docker-compose is run. This is
       needed so that the agent can reach into the config-service launched by
       this docker-compose file. If the host is not reachable, you may use the
        config service deployed in staging.
   
```
CONFIG_SERVICE_IP=10.20.30.40 CASTOR_DESCRIPTORS_LOC=/root/castor/descriptors CASTOR_OUTPUT_DIR=/root/castor/output docker-compose -f docker-compose-castor.yml up
```  

To point to the staging config service, use:
```
CONFIG_SERVICE_IP=https://cs-rest-stg.vdp-stg.vmware.com ... docker-compose -f docker-compose-castor.yml up
```