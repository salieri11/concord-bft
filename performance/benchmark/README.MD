# Performance Evaluation between Concord and Ethereum

In order to measure the performance of Concord, several Concord nodes (4 or 16), one Helen node, and one driver node are needed. Each node is basically an Ubuntu-16.04 VM with at least 2 CPUs. To proceed with the measurement, first set up the Blockchain SaaS (which includes the Concord nodes and Helen node), then run the performance measurement tool on driver node. For the 4-nodes Concord as an example, 4 VMs for the 4 Concord nodes, 1 VM for the Helen node and 1 VM for the driver node are required; total of 6 VMs. Please prepare the VMs before proceeding with the following steps. Concord VMs and Helen VM should have the same username (e.g. `blockchain`).

If setting up on CentOS, firewall has to be turned off on all nodes
```
systemctl disable firewalld
systemctl stop firewalld
```

## Prepare
1. Install docker on each of the VMs, follow [Get Docker CE for Ubuntu](https://docs.docker.com/install/linux/docker-ce/ubuntu/#os-requirements) (version 18.09.1)

2. Install docker-compose on each of the VMs, follow [Install Docker Compose](https://docs.docker.com/compose/install/)

3. Make sure password-less `ssh` is enabled to these VMs (concord, helen) from the driver node. Please follow [How to Use SSH Public Key Authentication](https://serverpilot.io/docs/how-to-use-ssh-public-key-authentication). This is to make sure you could `ssh` to those VMs with the public key authentication.


On the driver node, follow these steps to setup concord/helen nodes
## Environment setup

**NOTE:** The following commands are to be run as non-root user

1. Clone the blockchain repository to the driver VM
```
git clone git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git blockchain
```

2. Change directory to <blockchain repo>/performance and execute `setup_blockchain.sh` passing concord node IPs, helen IP, common username/password for these VMs

```bash
cd <blockchain repo>/performance/nodes
sh setup_blockchain.sh --concordIPs <concord IP1>,<concord IP2>,<concord IP3>,<concord IP4> --helenIP <helen IP> --username <username> --password '<password>'
```
**NOTE:**
Concord/helen VM's username/password is passed as arguments to the sript in order to run few commands as sudo
During the execution of the script, the script prompts for docker username/password


## Build Application project
**NOTE:** The following commands are to be run as non-root user


A build script can be used to build ballotApp and benchmark. To do it manually, follow the instructions after this.

```bash
cd <blockchain repo>/performance/benchmark
sh build.sh
```

After a successful build using the script, skip to step 3

1. On the driver VM, change directory to <blockchain repo>/performance/ballotApp and do a maven build of the `ballotApp` project. 
This requires maven (for Ubuntu, sudo apt install maven) and JDK (for Ubuntu, sudo apt install default-jdk) preinstalled on the driver VM.

A build script can be used to build ballotApp and benchmark. To do it manually, follow the instructions after this.
```bash
cd <blockchain repo>/performance/benchmark
sh build.sh
```
After a successful build using the script, skip to step 3

```bash
cd <blockchain repo>/performance/ballotApp
mvn clean install assembly:single
```

## Build benchmark project
This is a benchmark frame work in order to run all the workloads together given a yaml config file.
The config file include node information, number of threads, java executables of single test, and the output directory.
You need to setup your config file and put in the .config/ directory.

2. On the driver VM, change directory to <blockchain repo>/performance/benchmark and do a maven build of the `benchmark` project. 
   This requires maven (for Ubuntu, sudo apt install maven) and JDK (for Ubuntu, sudo apt install default-jdk) preinstalled on the driver VM.

```bash
cd <blockchain repo>/performance/benchmark
mvn clean package
```

3. To run the benchmark, change directory to <blockchain repo>/performance/benchmark and execute the following java command
**NOTE:** This requires JDK preinstalled on the driver VM (for Ubuntu, sudo apt install default-jdk)

The IPs for the 4 nodes are set in the config/BallotConfig2.yaml in the nodes section. In addition, each workload needs an IP where the contract will be deployed. At the moment this should be the 1st node.

```bash
cd <blockchain repo>/performance/benchmark
```
Connect to ethRPC on a concord node use the following command.
```bash
java -jar target/dapp-bench-1.0.0-jar-with-dependencies.jar config/BallotConfig2.yaml
```
## Sample Output

After the run the .csv file can be found in /perf_result/wordload0/

Starting DAppBench...
https://10.40.205.11:8545
21:24:37.481 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Connected to Ethereum client version: Helen/v1.1.0/linux/java1.8.0
21:24:42.395 [main] STAT  com.vmware.blockchain.performance.BallotDApp - STAT_DEPLOY_CONTRACT_LATENCY=604525849 ns
21:24:42.395 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Smart contract deployed to address 0xf0c17e337f45ad2fa0a8f406acd86feb657ea69f
21:24:42.395 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Transaction info about deploying contract:
TransactionReceipt{transactionHash='0x41e5823c2426563b561d405f3bb1dd8485d12ed66f02e46c06f81e8555a981b4', transactionIndex='0x0', blockHash='0x9d2c146df749738f38568ac7a0c2975b48fa0ae6ee181830e14315894dc157b7', blockNumber='0x1', cumulativeGasUsed='0x1c8d1', gasUsed='0x1c8d1', contractAddress='0xf0c17e337f45ad2fa0a8f406acd86feb657ea69f', root='null', status='0x1', from='0x857b630f3735665e4ad3d4785bf7837f714b3d50', to='0x', logs=[], logsBloom='0x00'}
21:24:45.047 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Total Time for Granting Right is 2191548648 nano seconds
21:24:45.047 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Number of threads: 4
21:24:45.047 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Number of Transactions: 500
21:24:47.654 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Time to create Voters is: 2606787263
21:24:47.655 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Starting Transactions..
21:24:48.903 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Finished executing all Transactions
21:24:48.906 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Concurrency Start Time is: 19437549417950
21:24:48.906 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Concurrency End time is: 19438797605461
21:24:48.907 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Concurrency Total Time is: 1248187511
21:24:48.907 [main] DEBUG com.vmware.blockchain.performance.BallotDApp - ./perf_result/workload0/wavefront/201907112124.log
21:24:48.912 [main] INFO  com.vmware.blockchain.performance.BallotDApp - File to dump data for wavefront is created!
21:24:48.976 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Average time response time: 45238.524
21:24:48.984 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Start time of processing Voting: 19437551591647
21:24:48.984 [main] INFO  com.vmware.blockchain.performance.BallotDApp - End time of processing Voting: 19438794721185
21:24:48.985 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Total time for process: 1243129538 nano seconds
21:24:48.985 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Start/End Transaction Rate: 402.2106986568925 tx/sec
21:24:48.986 [main] INFO  com.vmware.blockchain.performance.BallotDApp - Transaction Rate: 400.5893001896087 tx/sec



