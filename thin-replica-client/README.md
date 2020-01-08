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

The example application first retrieves initial state visible to the Thin
Replica Client via the `ReadState` and `ReadStateHash` RPC calls, which are
triggered by calling ThinReplicaClient::Subscribe function (which pushes all
initial state to the update queue before returning). Once the application has
exhausted the initial state, it will switch to waiting for additional updates to
arrive in the queue (via the subscription created by the
`ThinReplicaClient::Subscribe` call) and printing messages in real time as they
are received.

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

In order for the example application to successfully read any initial state from
a DAML cluster, the cluster needs application-level DAML state retrievable via
the Thin Replica mechanism. Running DAML-specific Hermes tests against your
cluster can provide a convenient way to trigger the creation of relevant state,
for example:

```
~/vmwathena_blockchain/hermes$ python3 main.py ThinReplicaTests --noLaunch
```

To run the Thin Replica Client example application in a container that can
connect to the cluster launched in the previous step:

```
~/vmwathena_blockchain$ docker run --rm --network docker_default trc:latest
```

Here is an example of what the initial output from the example application might
be expected to look like if everything works as expected:

```
2020-01-06T03:58:25.443 INFO  thin_replica.example: Attempting to construct ThinReplicaClient...
2020-01-06T03:58:25.443 INFO  thin_replica.example: ThinReplicaClient constructed.
2020-01-06T03:58:25.598 WARN  com.vmware.thin_replica_client: thin_replica_client::ThinReplicaClient::Subscribe is incomplete in its error handling and recovery; the worker thread Subscribe creates is also incomple in its error handling and recovery.
2020-01-06T03:58:25.598 INFO  thin_replica.example: ThinReplicaClient subscribed.
2020-01-06T03:58:25.598 INFO  thin_replica.example: The subscribe appears to have returned initial state to the update queue; fetching state from the update queue...
2020-01-06T03:58:25.598 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 1).
    The update appears to be empty.
2020-01-06T03:58:25.598 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 2).
    The update appears to be empty.
2020-01-06T03:58:25.598 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 3).
    The update appears to be empty.

<SOME LINES OMITTED FOR CONCISION>

2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 149).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 150).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 151).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 152).
    Update contains data for the following key(s) (not displaying values for concision):
        0x0a4065343931333532373838653536636134363033616363343131666665316134396665666437366564386231363361663836636635656535663463333836343562
        0x0a4030653631363766363730346465313333633861333531643535653939616136643062316233383734303162616265326366343566386235363433363865396162
        0x0a4064313465303833373466633731393764366130646534363863393638616538626133616164626639333135343736666433393037313833316635393233363632
        0x3a3a0801122437623563383462332d643038622d343934652d383963302d6432343736383865343435331a1064616d6c5f6c65646765725f61706931
        0x0a4035313830333266343166643031373534363162333561653063393639316530386234616561353565363239313566383336306166326363376131663262613663
        0x0a4031336666373335376530666564626561343465643864346639656462336363323464323265616138353535306437656131643165343263666565643937363864
        0x0a4063653561616332663631666331353462313736393662613962313166386530313530386237343861353935353162636661343032306331323266333430376162
        0x0a4034306634353232363062656633663239646564653133363130386663303861383864356135323530333130323831303637303837646136663062616464666637
        0x64616d6c2f313532
        0x0a4065326531393836306631646366663531363735323639636434343566653139313765393538346461386237303064333239646238376432343633363831356138
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 153).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 154).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 155).
    The update appears to be empty.

<SOME LINES OMITTED FOR CONCISION>

2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 198).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 199).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 199).
    The update appears to be empty.
2020-01-06T03:58:25.609 INFO  thin_replica.example: The (at least initial) contents of the update queue have been exhausted; will now wait for and report any additional updates...
```

Note that `<SOME LINES OMITTED FOR CONCISION>` is not actually expected in the
output; it has been added to this example as part of an edit to actual output to
reduce excessive repetition in the actual output.

The example application should not terminate on its own after this initial
output; furthermore, if any additional DAML state that ought to be visible to
the Thin Replica Client is generated, then additional output should be expected
in real time for the generated updates in the same format as the output for the
initial updates.
