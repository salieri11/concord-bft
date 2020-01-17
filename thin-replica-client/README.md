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

## Building the Thin Replica Client Library

The Thin Replica Client Library can be built with Docker. To build an image with
the header files and binaries needed to use this library (note this command is
to be run in the `vmwathena_blockchain` repository, one directory above where
this README is):

```
vmwathena_blockchain$ docker build -f thin-replica-client/Dockerfile . \
                                   -t trc-lib:latest
```

As it is anticipated applications consuming the Thin Replica Client Library will
be Dockerized, using Docker images of the Thin Replica Client Library built like
this as input to the Docker build of applications in order to obtain the
library's outward-facing header files and binaries is considered to be the
officially supported means of consuming this library at a build level.

CI for our project repository should regularly push builds of this image from
the `master` branch to Artifactory, tagged with a build number. Targeting a
specific artifact by build number as input to an application's Docker build is
recommended in order to maximize reproducability of application builds.

## Example Thin Replica Application

We provide an example application using the Thin Replica Client Library. The
example application is also intended to serve as a basic end-to-end test.

The example application first retrieves initial state visible to the Thin
Replica Client via the `ReadState` and `ReadStateHash` RPC calls, which are
triggered by calling ThinReplicaClient::Subscribe function (which pushes all
initial state to the update queue before returning); the application tries to
acknowledge the initial state after receiving it. Once the application has
exhausted the initial state, it will switch to waiting for additional updates to
arrive in the queue (via the subscription created by the
`ThinReplicaClient::Subscribe` call), printing messages in real time as they are
received, and trying to acknowledge those updates. In order to demonstrate and
test the `ThinReplicaClient::Unsubscribe` call and the graceful stop of a
subscription, the example application will automatically unsubscribe and exit
after a fixed number of updates are received.

It might be noted that update acknowledgement and unsubscription are not yet
supported end-to-end (server-side implementaitons are currently lacking);
however, placeholders are in place on the Thin Replica Client Library side, so
applications can already by written making these calls as that they ought to
make them once they run end-to-end. The Thin Replica Client example application
includes these calls to the library functions.

### Building and Running the Example Application

The example application is built with Docker from a Docker image of the Thin
Replica Client Library (see "Building the Thin Replica Client Library" above).
By default, the example application build targets a library build at
`trc-lib:latest`. If you have built or tagged a Thin Replica Client Library
image as such on your machine, then from the `vmwathena_blockchain` directory
(one directory above the `thin-replica-client` directory this README file is
in), the Thin Replica Client example application can be built with:

```
~/vmwathena_blockchain$ docker build . \
                                     -f thin-replica-client/DockerfileExample \
				     -t trc-example:latest
```

Docker build arguments can be used to target a different Docker image for the
build of the example application. For example, from the `vmwathena_blockchain`
directory (one above the `thin-replica-client` directory this README file is
in), the Thin Replica Client example application can be built targeting a Thin
Replica Client Library Image uploaded to
`athena-docker-local.artifactory.eng.vmware.com/trc-lib` with tag `1024` with:

```
~/vmwathena_blockchain$ docker build . \
                                     -f thin-replica-client/DockerfileExample \
				     -t trc-example:latest \
				     --build-arg \
         "trc_lib_repo=athena-docker-local.artifactory.eng.vmware.com/trc-lib" \
	                             --build-arg "trc_lib_tag=1024"
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
~/vmwathena_blockchain$ docker run --rm --network docker_default \
                                   trc-example:latest
```

Here is an example of what the initial output from the example application might
be expected to look like if everything works as expected:

```
2020-01-11T02:41:15.879 INFO  thin_replica.example: Attempting to construct ThinReplicaClient...
2020-01-11T02:41:15.880 INFO  thin_replica.example: ThinReplicaClient constructed.
2020-01-11T02:41:15.996 WARN  com.vmware.thin_replica_client: thin_replica_client::ThinReplicaClient::Subscribe is incomplete in its error handling and recovery; the worker thread Subscribe creates is also incomple in its error handling and recovery.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient subscribed.
2020-01-11T02:41:15.996 INFO  thin_replica.example: The subscribe appears to have returned initial state to the update queue; fetching state from the update queue...
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 1).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 2).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 3).
    The update appears to be empty.

<SOME LINES OMITTED FOR CONCISION>

2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 52).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 53).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 54).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 55).
    Update contains data for the following key(s) (not displaying values for concision):
        0x0a4064313465303833373466633731393764366130646534363863393638616538626133616164626639333135343736666433393037313833316635393233363632
        0x0a4065326531393836306631646366663531363735323639636434343566653139313765393538346461386237303064333239646238376432343633363831356138
        0x64616d6c2f3535
        0x0a4034306634353232363062656633663239646564653133363130386663303861383864356135323530333130323831303637303837646136663062616464666637
        0x0a4035313830333266343166643031373534363162333561653063393639316530386234616561353565363239313566383336306166326363376131663262613663
        0x0a4031336666373335376530666564626561343465643864346639656462336363323464323265616138353535306437656131643165343263666565643937363864
        0x0a4063653561616332663631666331353462313736393662613962313166386530313530386237343861353935353162636661343032306331323266333430376162
        0x0a4065343931333532373838653536636134363033616363343131666665316134396665666437366564386231363361663836636635656535663463333836343562
        0x0a4030653631363766363730346465313333633861333531643535653939616136643062316233383734303162616265326366343566386235363433363865396162
        0x3a3a0801122434313564633263322d323536302d343064332d393831392d3034323738653539653432321a1064616d6c5f6c65646765725f61706931
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 56).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 57).
    The update appears to be empty.
2020-01-11T02:41:15.996 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 58).
    The update appears to be empty.

<SOME LINES OMITTED FOR CONCISION>

2020-01-11T02:41:15.997 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 226).
    The update appears to be empty.
2020-01-11T02:41:15.997 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 227).
    The update appears to be empty.
2020-01-11T02:41:15.997 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 228).
    The update appears to be empty.
2020-01-11T02:41:15.997 INFO  thin_replica.example: The (at least initial) contents of the update queue have been exhausted; will now wait for and report any additional updates...
2020-01-11T02:41:15.997 INFO  thin_replica.example: Update(s) acknowledged.
2020-01-11T02:41:16.001 INFO  thin_replica.example: This example application will wait for 64 updates before trying to stop and restart the subscription, then wait for an additional 64 updates before trying to permanently unsubscribe...
```

Note that `<SOME LINES OMITTED FOR CONCISION>` is not actually expected in the
output; it has been added to this example as part of an edit to actual output to
reduce excessive repetition in the actual output.

The example application should not immediately terminate on its own after this
initial output; if any additional DAML state that ought to be visible to the
Thin Replica Client is generated, then additional output should be expected in
real time for the generated updates. The example application will wait to
receive a fixed number of updates before attempting to unsubscribe and
terminating (128 updates in the example output here; which sounds like a lot,
though it should not take too long as the time service causes the generation of
a steady stream of empty updates). In order to demonstrate the Thin Replica
Client Library's support for resuming a stopped subscription via a saved Block
ID, the example application will destroy and reconstruct its Thin Replica Client
object half way through this fixed number of updates, then try to re-subscribe
with the last Block ID it received before destroying its first client.

Here is an example of what some of the additional output from updates received in real time might look like:

```
2020-01-11T02:41:24.397 INFO  thin_replica.example: Acknowledged update with with Block ID 261.
2020-01-11T02:41:24.420 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 262).
    The update appears to be empty.
2020-01-11T02:41:24.420 INFO  thin_replica.example: Acknowledged update with with Block ID 262.
2020-01-11T02:41:24.517 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 263).
    The update appears to be empty.
2020-01-11T02:41:24.517 INFO  thin_replica.example: Acknowledged update with with Block ID 263.
2020-01-11T02:41:24.528 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 264).
    The update appears to be empty.
2020-01-11T02:41:24.528 INFO  thin_replica.example: Acknowledged update with with Block ID 264.
2020-01-11T02:41:25.523 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 265).
    Update contains data for the following key(s) (not displaying values for concision):
        0x3a3a0801122461643839636532382d356136652d346165322d383635342d3730306230383535376232611a1064616d6c5f6c65646765725f61706931
        0x64616d6c2f323635
2020-01-11T02:41:25.523 INFO  thin_replica.example: Acknowledged update with with Block ID 265.
2020-01-11T02:41:25.806 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 266).
    The update appears to be empty.
2020-01-11T02:41:25.806 INFO  thin_replica.example: Acknowledged update with with Block ID 266.
2020-01-11T02:41:25.807 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 267).
    The update appears to be empty.
2020-01-11T02:41:25.807 INFO  thin_replica.example: Acknowledged update with with Block ID 267.
2020-01-11T02:41:25.807 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 268).
    The update appears to be empty.
```

Here is an example of what the point in the output might look like at which the
example application destroys and reconstructs its Thin Replica Client object and
tries to resume its existing subscription:

```
2020-01-17T02:26:50.876 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 267).
    The update appears to be empty.
2020-01-17T02:26:50.876 INFO  thin_replica.example: Acknowledged update with with Block ID 267.
2020-01-17T02:26:51.063 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 268).
    The update appears to be empty.
2020-01-17T02:26:51.063 INFO  thin_replica.example: Acknowledged update with with Block ID 268.
2020-01-17T02:26:51.085 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 269).
    The update appears to be empty.
2020-01-17T02:26:51.085 INFO  thin_replica.example: Acknowledged update with with Block ID 269.
2020-01-17T02:26:51.107 INFO  thin_replica.example: Received 64 updates; restarting subscription...
2020-01-17T02:26:51.892 INFO  thin_replica.example: Destroyed ThinReplicaClient object in use.
2020-01-17T02:26:51.893 INFO  thin_replica.example: New ThinReplicaClient object constructed.
2020-01-17T02:26:51.893 WARN  com.vmware.thin_replica_client: thin_replica_client::ThinReplicaClient::Subscribe is incomplete in its error handling and recovery; the worker thread Subscribe creates is also incomple in its error handling and recovery.
2020-01-17T02:26:51.893 INFO  thin_replica.example: Subscription resumed from block 269; will wait for an additional 64 updates before trying to permanently unsubscribe...
2020-01-17T02:26:51.900 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 269).
    The update appears to be empty.
2020-01-17T02:26:51.900 INFO  thin_replica.example: Acknowledged update with with Block ID 269.
2020-01-17T02:26:51.900 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 270).
    The update appears to be empty.
2020-01-17T02:26:51.900 INFO  thin_replica.example: Acknowledged update with with Block ID 270.
2020-01-17T02:26:51.900 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 271).
    The update appears to be empty.
```

Here is an example of how the output might conclude once the example application
receives its fixed number of updates and unsubscribes:

```
2020-01-11T02:41:47.180 INFO  thin_replica.example: Acknowledged update with with Block ID 352.
2020-01-11T02:41:47.759 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 353).
    The update appears to be empty.
2020-01-11T02:41:47.759 INFO  thin_replica.example: Acknowledged update with with Block ID 353.
2020-01-11T02:41:48.140 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 354).
    The update appears to be empty.
2020-01-11T02:41:48.141 INFO  thin_replica.example: Acknowledged update with with Block ID 354.
2020-01-11T02:41:48.150 INFO  thin_replica.example: ThinReplicaClient reported an update (Block ID: 355).
    The update appears to be empty.
2020-01-11T02:41:48.150 INFO  thin_replica.example: Acknowledged update with with Block ID 355.
2020-01-11T02:41:48.192 INFO  thin_replica.example: Received 64 updates; unsubscribing...
2020-01-11T02:41:48.773 INFO  thin_replica.example: ThinReplicaClient unsubscribed.
```
