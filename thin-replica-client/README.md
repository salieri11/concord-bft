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

At the time of this writing, the example application only retrieves initial
state visible to the Thin Replica Client via the ReadState and ReadStateHash RPC
calls, which are triggered by calling ThinReplicaClient::Subscribe function.

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

In order for the example application to successfully read the state from a DAML
cluster, the cluster needs application-level DAML state retrievable via the Thin
Replica mechanism. Running DAML-specific Hermes tests against your cluster can
provide a convenient way to trigger the creation of relevant state, for example:

```
~/vmwathena_blockchain/hermes$ python3 main.py ThinReplicaTests --noLaunch
```

To run the Thin Replica Client example application in a container that can
connect to the cluster launched in the previous step:

```
~/vmwathena_blockchain$ docker run --rm --network docker_default trc:latest
```

Here is an example of what the output from the example application might be expected to look like if everything works as expected:

```
2019-12-17T20:14:52.139 INFO  thin_replica.example: Attempting to construct ThinReplicaClient...
2019-12-17T20:14:52.139 INFO  thin_replica.example: ThinReplicaClient constructed.
2019-12-17T20:14:52.421 WARN  com.vmware.thin_replica_client: concord::thin_replica_client::ThinReplicaClient::Subscribe is incompletely implemented.
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient subscribed.
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4037323034386538383565326563306330356561613438363632666261353937656231303266306233393065323362393839623735343436326135353733656235
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4035313830333266343166643031373534363162333561653063393639316530386234616561353565363239313566383336306166326363376131663262613663
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4034306634353232363062656633663239646564653133363130386663303861383864356135323530333130323831303637303837646136663062616464666637
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4063653561616332663631666331353462313736393662613962313166386530313530386237343861353935353162636661343032306331323266333430376162
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4065343931333532373838653536636134363033616363343131666665316134396665666437366564386231363361663836636635656535663463333836343562
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4030653631363766363730346465313333633861333531643535653939616136643062316233383734303162616265326366343566386235363433363865396162
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x64616d6c2f313934
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 302).
Update contains data for the following key(s) (not displaying values for concision):
  0x0a4034663965363461636636386564306436663630613237326538656266366435356435326134613332343438653732303866336332306231376261373930343137
2019-12-17T20:14:52.421 FATAL com.vmware.thin_replica_client: concord::thin_replica_client::ThinReplicaClient::AcknowledgeBlockID is unimplemented.
2019-12-17T20:14:52.421 INFO  thin_replica.example: Update(s) acknowledged.
2019-12-17T20:14:52.421 FATAL com.vmware.thin_replica_client: concord::thin_replica_client::ThinReplicaClient::Unsubscribe is unimplemented.
2019-12-17T20:14:52.421 INFO  thin_replica.example: ThinReplicaClient unsubscribed.

```
