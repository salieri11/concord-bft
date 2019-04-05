# Performance Evaluation between Concord and Ethereum
![Performance Environment Overview](performance_environment.jpg?raw=true "Performance Environment Overview")

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
git clone git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git
```

2. Change directory to <blockchain repo>/performance and execute `setup_blockchain.sh` passing concord node IPs, helen IP, common username/password for these VMs

```bash
cd <blockchain repo>/performance/nodes
sh setup_blockchain.sh --concordIPs <concord IP1>,<concord IP2>,<concord IP3>,<concord IP4> --helenIP <helen IP> --username <username> --password '<password>'
```
**NOTE:**
Concord/helen VM's username/password is passed as arguments to the sript in order to run few commands as sudo
During the execution of the script, the script prompts for docker username/password


## Build Performance project

**NOTE:** The following commands are to be run as non-root user

1. On the driver VM, change directory to <blockchain repo>/performance and do a maven build of the `performance` project. 
This requires maven (for Ubuntu, sudo apt install maven) and JDK (for Ubuntu, sudo apt install default-jdk) preinstalled on the driver VM.

```bash
cd <blockchain repo>/performance
mvn clean install assembly:single
```

2. To run the Performance Test, change directory to <blockchain repo>/performance and execute the following java command
**NOTE:** This requires JDK preinstalled on the driver VM (for Ubuntu, sudo apt install default-jdk)
```
cd <blockchain repo>/performance
java -Xmx1g -jar target/performance-1.0-SNAPSHOT-jar-with-dependencies.jar <HELEN_IP> 1000 data/proposals ~/result
```
Here the parameter <HELEN_IP> is the IP of helen VM,
`1000` is the number of concurrent voting,
`proposals` is the file path of the proposals,
`result` is the result path of the performance test 


