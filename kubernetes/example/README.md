# Kubernetes Example (Unsupported Configuration)
> **DISCLAIMER: (BEFORE YOU START READING THE REST OF THE DOCUMENT)**
>
> Kubernetes is an UNSUPPORTED product deployment at time of this writing.
> The example is provided AS-IS to demonstrate the viability of deploying bits
> and pieces of the project in Kubernetes - in a way that mimics the analogous
> deployment workflow for the actual product.

There is a starter example that sets up a Concord cluster of 4 replica nodes as
4 separate deployments. Each deployment has a headless service exposing its
Concord SBFT communication ports for the other replica nodes in the cluster.
Each deployment also has an Ethream RPC serving sidecar container that is
coupled with the replica container instance. To expose the Concord cluster's
Ethereum RPC serving instances to the Kubernetes cluster via a stable network-
addressable entity, there is an additional Ethereum RPC service object per-
replica that exposes only the ethreum RPC port.

> This guide assumes that there is already a working Kubernetes cluster and
> that the local working environment has `kubectl` setup and configured to
> communicate with the Kubernetes cluster as its context.

## Example File Structure
Under `example/`, there are 5 `.yaml` files:
1. `node1.yaml`: Deployment pod for node1 replica + ethereum RPC side-car.
2. `node2.yaml`: Deployment pod for node2 replica + ethereum RPC side-car.
3. `node3.yaml`: Deployment pod for node3 replica + ethereum RPC side-car.
4. `node4.yaml`: Deployment pod for node4 replica + ethereum RPC side-car.

There is also a `configmap` directory that contains the cluster's configuration
files. The content should be bundled in a Kubernetes `ConfigMap` object.

## Setup
1. Build the requisite artifacts. There should be a `concord:k8s` and
   `ethrpc:k8s` image in the docker registry or local images accessible
   to the target Kubernetes cluster. Follow the general README instruction
   to build a `concord` and `ethrpc` image, and tag them with `k8s` tag
   designation.

   Example:
```
blockchain$ docker build . -f concord/Dockerfile -t concord:k8s

blockchain$ docker build . -f ethrpc/Dockerfile -t ethrpc:k8s
```

2. Create the `ConfigMap` object to be referenced by the deployments and
   mounted as `config` volumes.
```
example$ kubectl create configmap --from-file=configmap

configmap "concord-example-config" created
```

3. Deploy the cluster replicas.
```
example$ kubectl create -f node1.yaml -f node2.yaml -f node3.yaml -f node4.yaml

service "concord-example-node1-ethrpc" created
service "concord-example-node1" created
deployment.apps "concord-example-node1" created
service "concord-example-node2-ethrpc" created
service "concord-example-node2" created
deployment.apps "concord-example-node2" created
service "concord-example-node3-ethrpc" created
service "concord-example-node3" created
deployment.apps "concord-example-node3" created
service "concord-example-node4-ethrpc" created
service "concord-example-node4" created
deployment.apps "concord-example-node4" created
```

At this point the all deployments and services should be deployed. Wait for
everything to come up. To verify, a resource list should resemble something
similar to the following:
```
example$ kubectl get all -o wide
NAME                                         READY     STATUS    RESTARTS   AGE       IP           NODE
pod/concord-example-node1-5d5cb6cc79-lj8hf   2/2       Running   0          4m        10.1.0.134   k8s-worker-1
pod/concord-example-node2-bc8cf4c8d-xfgtq    2/2       Running   0          4m        10.1.0.133   k8s-worker-1
pod/concord-example-node3-bcc7d77bc-kl8k5    2/2       Running   0          4m        10.1.0.135   k8s-worker-2
pod/concord-example-node4-55bf78946b-8fmn9   2/2       Running   0          4m        10.1.0.136   k8s-worker-2

NAME                                   TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)                                        AGE       SELECTOR
service/concord-example-node1          ClusterIP   None            <none>        3501/UDP,3502/UDP,3503/UDP,3504/UDP,3505/UDP   4m        app=example-node1
service/concord-example-node1-ethrpc   NodePort    10.96.225.52    <none>        8545:31200/TCP                                 4m        ethrpc=example-node1
service/concord-example-node2          ClusterIP   None            <none>        3501/UDP,3502/UDP,3503/UDP,3504/UDP,3505/UDP   4m        app=example-node2
service/concord-example-node2-ethrpc   NodePort    10.102.176.52   <none>        8545:31681/TCP                                 4m        ethrpc=example-node2
service/concord-example-node3          ClusterIP   None            <none>        3501/UDP,3502/UDP,3503/UDP,3504/UDP,3505/UDP   4m        app=example-node3
service/concord-example-node3-ethrpc   NodePort    10.109.151.20   <none>        8545:31070/TCP                                 4m        ethrpc=example-node3
service/concord-example-node4          ClusterIP   None            <none>        3501/UDP,3502/UDP,3503/UDP,3504/UDP,3505/UDP   4m        app=example-node4
service/concord-example-node4-ethrpc   NodePort    10.109.69.183   <none>        8545:32647/TCP                                 4m        ethrpc=example-node4
service/kubernetes                     ClusterIP   10.96.0.1       <none>        443/TCP                                        4d        <none>

NAME                                    DESIRED   CURRENT   UP-TO-DATE   AVAILABLE   AGE       CONTAINERS                       IMAGES                   SELECTOR
deployment.apps/concord-example-node1   1         1         1            1           4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node1
deployment.apps/concord-example-node2   1         1         1            1           4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node2
deployment.apps/concord-example-node3   1         1         1            1           4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node3
deployment.apps/concord-example-node4   1         1         1            1           4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node4

NAME                                               DESIRED   CURRENT   READY     AGE       CONTAINERS                       IMAGES                   SELECTOR
replicaset.apps/concord-example-node1-5d5cb6cc79   1         1         1         4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node1,pod-template-hash=1817627735
replicaset.apps/concord-example-node2-bc8cf4c8d    1         1         1         4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node2,pod-template-hash=674790748
replicaset.apps/concord-example-node3-bcc7d77bc    1         1         1         4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node3,pod-template-hash=677383367
replicaset.apps/concord-example-node4-55bf78946b   1         1         1         4m        concord-replica,concord-ethrpc   concord:k8s,ethrpc:k8s   app=example-node4,pod-template-hash=1169345026
```

Note the cluster IP for the exposed Ethereum RPC service. We can verify cluster
is functional by submitting an ethereum RPC against the RPC service IP. To do
this, we can spin up a command-line pod instance:
```
example$ kubectl run cmd --rm --image=ubuntu:18.04 -it -- /bin/bash

root@cmd-7cfbc6cfc9-zphwm:/# apt-get update && apt-get install -y curl
Get:1 http://security.ubuntu.com/ubuntu bionic-security InRelease [83.2 kB]
Get:2 http://archive.ubuntu.com/ubuntu bionic InRelease [242 kB]
...
Running hooks in /etc/ca-certificates/update.d...
done.

root@cmd-7cfbc6cfc9-zphwm:/# curl -X POST -H "content-type: application/json" 'http://concord-example-node1-ethrpc:8545/' --data '{"jsonrpc":"2.0","id":1,"method":"eth_getBlockByNumber","params":["0x0", true]}'
{"result":{"number":"0x0","gasLimit":"0xf4240","size":1,"parentHash":"0x0000000000000000000000000000000000000000000000000000000000000000","transactions":[{"blockHash":"0x2a39afdcc70b4e494db09486c500c5ec1f956b3b28e6576b47c3fb4c4a0c0fe4","input":"0x","blockNumber":"0x0","contractAddress":"0x","transactionIndex":"0x0","from":"0x0000000000000000000000000000000000000000","to":"0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019","nonce":"0x0","value":"0x12345","logs":[],"hash":"0xb0832c41b0da7a95081828dcc1c2c5c5ed7fe9d4622dbf2d12dc02fadc86f995"},{"blockHash":"0x2a39afdcc70b4e494db09486c500c5ec1f956b3b28e6576b47c3fb4c4a0c0fe4","input":"0x","blockNumber":"0x0","contractAddress":"0x","transactionIndex":"0x0","from":"0x0000000000000000000000000000000000000000","to":"0x5bb088f57365907b1840e45984cae028a82af934","nonce":"0x1","value":"0x11259375","logs":[],"hash":"0x9e7415d9b64e4bbf02b36088fd04f2a33281bd4036c2395e82afd1c74696a4d1"}],"nonce":"0x0000000000000000000000000000000000000000000000000000000000000000","hash":"0x2a39afdcc70b4e494db09486c500c5ec1f956b3b28e6576b47c3fb4c4a0c0fe4","timestamp":"0x0"},"id":1,"jsonrpc":"2.0"}
```

## Limitations
Even though this formulation of a 4-node Concord cluster deployed in Kubernetes
demonstrates a multi-node deployment, it does not actually survive individual
deployment pod lifecycle events.

Specifically, currently Concord cluster over UDP communication relies on a
stable IP address for all participating replicas. Each replica's network
address is baked into a startup configuration set, which is populated within
the start command of each `concord-replica` container instance:

```yaml
containers:
- name: concord-replica
  image: concord:k8s
  command:
  - "/bin/sh"
  - "-c"
  - >
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib &&
    /concord/config/network-settings.sh /concord/config/local/s_f1c0_config.pub concord1 concord-example-node1 &&
    /concord/config/network-settings.sh /concord/config/local/s_f1c0_config.pub concord2 concord-example-node2 &&
    /concord/config/network-settings.sh /concord/config/local/s_f1c0_config.pub concord3 concord-example-node3 &&
    /concord/config/network-settings.sh /concord/config/local/s_f1c0_config.pub concord4 concord-example-node4 &&
    /concord/concord -c /concord/config/concord.config
```

`network-settings.sh` waits until `nslookup` for each Concord replica
deployment (e.g. `concord-example-node1`) to resolve via Kubernetes DNS, and
then populates the configuration with the resolved IP address. Since this
information set is only accurately reflecting deployment information at
startup, the configuration setup does not take into account the fact that
deployments can be scaled down (to 0) and back up (to 1), or endure
other lifecycle actions that may occur to a Kubernetes pod, inter-replica
communication will break if the replica deployment's IP changes.

> Note: While it should be possible for Kubernetes to provide a stable IP
> via a service object, Concord SBFT's current UDP communication module
> derives sender information from UDP packet's SRC field. So while Kubernetes
> service objects takes care of network traffic routing, it does not perform
> network address translation of the network payload. Because of this,
> Concord SBFT's UDP communication module will not associate the incoming
> UDP packets with the associated SBFT replica ID => inter-replica is
> non-communicating when relying on Kubernetes service objects to
> direct SBFT-related UDP traffic.
