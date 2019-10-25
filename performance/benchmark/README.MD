# Performance Evaluation between Concord and Ethereum

In order to measure the performance of Concord, several Concord nodes (4 or 7) and one driver node are needed. 
First set up the Blockchain SaaS through the deployment service, 
then run the performance measurement tool on driver node. 

If testing the performance of daml workloads, you will have a concord cluster setup which has daml-ledger-api installed on every node. Also, make sure the QuickStart DApp is deployed to the DAML ledger as described [here](https://docs.daml.com/deploy/index.html).

On the driver node, follow these steps to setup environments
## Environment setup

**NOTE:** The following commands are to be run as non-root user
If you will be running workloads of daml apps,

**Install DAML SDK (Optional)** 


```
curl -sSL https://get.daml.com/ | sh
```
If prompted, add ~/.daml/bin to your PATH.

Otherwise, skip the above step.

**Clone the blockchain repository to the driver VM**
```
git clone git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git blockchain
```

**Build the benchmark project**

On the driver VM, change directory to <blockchain repo>/performance/benchmark and do a maven build of the `benchmark` project. 
This requires maven (for Ubuntu, sudo apt install maven) and JDK (for Ubuntu, sudo apt install default-jdk) preinstalled on the driver VM.

A build script can be used to build benchmark project and projects inside it automatically. To do it manually, follow the instructions after this.
```bash
cd <blockchain repo>/performance/benchmark
./build.sh
```




## Build benchmark project manually

**Build the ballotApp**
```bash
cd <blockchain repo>/performance/ballotApp
mvn clean install assembly:single
```

**Build the benchmark project**
```bash
cd <blockchain repo>/performance/benchmark
mvn clean install assembly:single
```

**Run the benchmark**

Change directory to <blockchain repo>/performance/benchmark and execute the following java command

**NOTE:** 

This requires JDK preinstalled on the driver VM (for Ubuntu, sudo apt install default-jdk)

The IPs for the 4 nodes are set in the config/BallotConfig2.yaml in the nodes section. In addition, each workload needs an IP where the contract will be deployed. At the moment this should be the 1st node.

```bash
cd <blockchain repo>/performance/benchmark
```
Start the benchmark tool
```bash
java -jar target/dapp-bench-1.0.0-jar-with-dependencies.jar config/<config-yml>
```
Refer [config](config) for examples. 

## Sample Output for Ethereum

After the run the .csv file can be found in /perf_result/wordload0/

```
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
```

## Sample Output for DAML


```
Starting DAppBench...
13:22:36.025 [main] INFO  dappbench.BenchMaster - Got config file: config/4n-IOUConfig.yaml
13:22:36.092 [main] INFO  dappbench.BenchMaster - workloads size: 1
13:22:36.092 [main] INFO  dappbench.BenchMaster - Running IOU workloads
13:22:36.095 [main] INFO  dappbench.DAMLManager - Total number of Transactions 1000
13:22:36.095 [main] INFO  dappbench.DAMLManager - Rate control value: 200
13:22:36.884 [main] INFO  dappbench.DAMLManager - Expected distribution: {10.40.205.84=280, 10.40.205.86=500, 10.40.205.83=220}
13:22:36.886 [main] INFO  dappbench.DAMLManager - Starting transaction ...
13:22:43.087 [main] INFO  dappbench.DAMLManager - Total time taken: 6201 ms
13:22:43.087 [main] INFO  dappbench.DAMLManager - Average latency: 6 ms
13:22:43.088 [main] INFO  dappbench.DAMLManager - Throughput: 161 tps
13:22:43.091 [main] INFO  dappbench.DAMLManager - Nodes: {10.40.205.83=220, 10.40.205.84=280, 10.40.205.86=500}
13:22:43.091 [main] INFO  dappbench.BenchMaster - Finished benchmark!
```