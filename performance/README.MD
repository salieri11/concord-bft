# Performance Evaluation between Athena and Ethereum
![Performance Environment Overview](performance_environment.jpg?raw=true "Performance Environment Overview")

In order to measure the performance of Concord (previously called Athena, here we use them interchangeably), we need several Concord nodes (4 or 16), one Helen node, one driver node. Each node is basically an Ubuntu-16.04 VM with at least 2 CPUs. To proceed with the measurement, we should at first set up the Blockchain SaaS (which includes the Concord nodes and Helen node),  then run the performance measurement tool on driver node. Take the 4-nodes Concord as example, you need 4 VMs for the 4 Concord nodes, 1 VM for the Helen node and 1 VM for the driver node, totally 6 VMs you will need. Please prepare the VMs before proceeding with the following steps.
The Concord VMs should have the same username (e.g. `concord`).


## Prepare
1. Install docker on each of the VMs, follow [Get Docker CE for Ubuntu](https://docs.docker.com/install/linux/docker-ce/ubuntu/#os-requirements) (version 18.09.1)

2. Install docker-compose on each of the VMs, follow [Install Docker Compose](https://docs.docker.com/compose/install/)

3. On the Athena node VMs, get athena image
```bash
docker login --username=yourusername --password=yourpassword
docker pull beerriot/p2bc:athena20181016
docker tag beerriot/p2bc:athena20181016 athena:latest
```
if you get login denial, please try ```sudo usermod -a -G docker $USER``` and relogin to the VM/terminal

4. On the Helen node VM, get helen image
```bash
docker login --username=yourusername --password=yourpassword
docker pull beerriot/p2bc:helen20181016
docker tag beerriot/p2bc:helen20181016 helen:latest
```

5. Make sure you could `ssh` to those VMs (athena, helen, driver) from your local machine. Please follow [How to Use SSH Public Key Authentication](https://serverpilot.io/docs/how-to-use-ssh-public-key-authentication). This is to make sure you could `ssh` to those VMs with the public key authentication.


Start from here, all the following operations are executed on your local machine unless otherwise stated.
## Environment setup for Athena
1. Clone the repository to your local machine
```
git clone git@github.com:vmwathena/performance.git
```

2. Get the internal IP of each Athena VM. Add the internal IPs to the file `performance-repo/nodes/internal_ip.txt`. One IP per line. Internal IP means the IP address you could access within LAN.

The content in internal_ip.txt should look like:
```
10.193.27.221
10.193.28.153
10.193.15.245
10.193.30.157
```

Also change the HOSTNAMES in the file `performance-repo/nodes/config-public/find-docker-instances.sh` to the internal IPs, which are the same IPs mentioned in the previous step, e.g.
```bash
    HOSTNAMES=("10.193.27.221" \
                "10.193.28.153" \
                "10.193.15.245" \
                "10.193.30.157")
```

Update line 17 in file `performance-repo/nodes/config-public/find-docker-instances.sh` to use the correct configuration file. If working on 4-nodes configuration, please change it to
```
PUBPATH=/athena/config-local/s_f1c0_config.pub
```
otherwise, for 16-nodes configuration, please change it to
```
PUBPATH=/athena/config-local/s_f5c0_config.pub
```

3. Generate config-public. You must have a jdk installed before running this step (e.g., for Ubuntu, sudo apt install default-jdk). 
```bash
cd performance-repo
mkdir bin
javac -d bin -sourcepath src src/main/java/com/vmware/blockchain/performance/ConfigNodes.java
java -cp bin com.vmware.blockchain.performance.ConfigNodes <NUMBER_OF_CONCORD_NODES>
```
Replace <NUMBER_OF_CONCORD_NODES> with 4 (or 16).

4. Get the external IP of each Athena VM. Add the external IPs to the file `performance-repo/nodes/external_ip.txt`. One IP per line. External IP means the IP address you could access outside LAN. If your local machine is within the same LAN of those VMs, the external IPs could be the same as the internal IPs.
   ```bash
   performance
   |-- nodes
       |-- external_ip.txt
   ```
   The content in external_ip.txt should look like:
   ```
   10.193.27.221
   10.193.28.153
   10.193.15.245
   10.193.30.157
   ```

5. Execute setup_athena.sh to deploy athena nodes
```bash
cd performance-repo/nodes
./setup_athena.sh 4 external_ip.txt concord
```
Here the parameter `4` means 4-nodes configuration, 
`external_ip.txt` is the path to external.txt file, 
`concord` is the username of each concord VM, it is used by`ssh` in `setup_athena.sh`

Note: During execution, the script would prompt for concord_user's password for each concord VM interactively, as certain commands need sudo access


## Setup Helen Node

1. On the local VM, Edit `performance-repo/nodes/application.properties` to replace the value of `AthenaAuthorities` at line 4 with internal IPs of Athena VMs. This should look like
```
AthenaAuthorities=10.193.27.221:5458,10.193.28.153:5458,10.193.15.245:5458,10.193.30.157:5458
```

2. To deploy helen on a VM, Replace <HELEN_IP> with the IP assigned for helen.
```
cd performance-repo/nodes
./helen.sh <HELEN_IP> <HELEN_VM_USERNAME>
```
here the parameter <HELEN_IP> is the IP of Helen VM,
<HELEN_VM_USERNAME> is the username on Helen VM

## Setup driver node
1. On the local VM, compile the driver code. You must have maven installed before running this step (e.g., for Ubuntu, sudo apt install maven).
   ```bash
   cd performance-project-path
   mvn clean install assembly:single
   ```

2. Copy the file `performane-repo/data/proposals` to the driver VM, replace driver_name and driver_host in the following command with the actuals.
```
scp performance-repo/data/proposals ${driver_user}@${driver_host}:/home/${driver_user}/
```

3. Copy performance-1.0-SNAPSHOT-jar-with-dependencies.jar to driver node
```
scp performance-repo/target/performance-1.0-SNAPSHOT-jar-with-dependencies.jar ${driver_user}@${driver_host}:/home/${driver_user}
```

4. `ssh` to driver VM, run the driver program on driver node to get all the measurements
```
java -Xmx1g -jar performance-1.0-SNAPSHOT-jar-with-dependencies.jar <HELEN_IP> 1000 proposals result
```
Here the parameter <HELEN_IP> is the IP of helen VM,
`1000` is the number of concurrent voting,
`proposals` is the file path of the proposals,
`result` is the result path of the performance test 
