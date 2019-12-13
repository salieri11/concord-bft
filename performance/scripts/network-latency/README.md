# Network Latency Statistics

## Motivation

As all the concord replica nodes communicate with each other for consensus, it is important to measure the overhead of network between these nodes.

## Collect Stats

### Overview

First all the replicas nodes are discovered using the configuration of the given node. Then each node sends multiple ICMP echo requests to other replica nodes. The raw statistics output is saved in files with name format \<source\>_\<target\>. The script takes care of opening firewall on each node to accept PING requests and closes at end of it.  

### Prerequisites

Set environment variable for the *root* user password.

```bash
sudo apt install sshpass
export SSHPASS=xxxx
```
### Usage

./[collect-stats.sh](collect-stats.sh) \<node\>

*node* - IP address of one of the concord replica nodes

### Output

Under directory *raw*, directories corresponding to each source node is created. Under each source node, files for each target nodes are created.  

## Show stats

### Overview

All the raw PING statistics are parsed into JSON and TSV format.

### Prerequisites

```bash
sudo apt install python-pip
pip install pingparsing
PATH=$PATH:~/.local/bin

sudo apt install jq
```

### Usage

./[show-stats.sh](show-stats.sh)

### Output

Under the current directory, stats.json is created. The TSV output, sorted by rtt_avg is shown in the CLI itself.