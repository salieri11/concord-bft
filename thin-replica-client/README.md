# Thin Replica Client Library

Thin Replicas are a mechanism that may be used in VMware Blockchain Deployments
to support subscription-based streaming of updates from committer nodes (Concord
replica nodes) participating in consensus to participant nodes that do not
participate in consensus but host parties observing and/or transacting in the
blockchain maintained by the Concord cluster. The thin replica mechanism is also
designed to support limiting what updates the committer nodes allow a particular
participant node to see.

Please see the Thin Replica design spec on Confluence for a more detailed
explanation of the thin replica mechanism.

The thin replica mechanism uses a client/server model. The thin replica server
is a part of Concord that runs on the committer node. As the thin replica
mechanism is designed, the clients may be arbitrary application(s) that run on
participant nodes; we provide the Thin Replica Client Library for use by such
applications.

The interface to the Thin Replica Client Library is defined in the header file
`thin-replica-client/lib/thin_replica_client.hpp`. Comments with the definitions
in this file provide primary documentation of the interface.

At the time of this writing, the Thin Replica Client Library is still under
development and has not yet been fully implemented.

## Example Thin Replica Application

We provide an example application using the Thin Replica Client Library. The
example application is also intended to serve as a basic end-to-end test.

At the time of this writing, the example application only verifies end-to-end
connectivity of the thin replica mechanism.

From the `vmwathena_blockchain directory` (one directory above the
`thin-replica-client` directory this README file is in), the Thin Replica Client
example application can be built with:

```
~/vmwathena_blockchain$ docker build . -f thin-replica-client/Dockerfile \
                                       -t trc:latest
```

To run the example Thin Replica Client application and connect it to a local
Concord development cluster:

Configure and launch the DAML Nano `docker-compose` cluster:

```
~/vmwathena_blockchain/docker$ ./gen-docker-concord-config.sh \
        config-public/dockerConfigurationInput-daml-nano.yml
~/vmwathena_blockchain/docker$ docker-compose -f docker-compose-daml-nano.yml up
```

Clearing the contents of `docker/devdata` is recommended after reconfiguring the
cluster.

To run the Thin Replica Client example application in a container that can
connect to the cluster launched in the previous step:

```
~/vmwathena_blockchain$ docker run --rm --network docker_default trc:latest
```

At the time of this writing, the output from running the Thin Replica Client
example applicaiton should be something like:

```
2019-12-04T05:01:51.785 INFO  thin_replica.example: Attempting to construct ThinReplicaClient...
2019-12-04T05:01:51.785 INFO  thin_replica.example: ThinReplicaClient constructed.
2019-12-04T05:01:51.862 ERROR thin_replica.example: An exception occurred while trying to construct a ThinReplicaClient and connect it to the Thin Replica Server(s). Exception message:
ThinReplicaClient subscription failed: response to ReadStateRequest from ThinReplicaServer gave a failure status (error code: 12, error message: "ReadState").
```
