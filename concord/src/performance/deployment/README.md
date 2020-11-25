# Performance Execution Engine deployment

## This document describes steps for deploying Concord with Performance Execution Engine to AWS

### Motivation
Performance Execution Engine (PEE) allows to run Concord cluster without running actual execution environment. The reason is to make performance evaluation easier. Engineers used to evaluate performance on the single machine, with all benefits of the simplicity and complete isolation from network latencies. However, we realise that having ability to run the system on real machines with real network is beneficial for the system.

### Performance Execution Engine
The PEE basically is a subclass of ConcordRequestHandler, which allows to create write sets of various sizes and to simulate 0 execution time. It allows to measure end to end latency which covers communication, consensus, KVBC operations and storage times.
Each replica loads the execution handler based on the value from the configuration file, and this deployment tool sets the value to PEE by default.

### Deployment configuration file
`deployment.yaml` file in the current folder is just an example of the configuration file. Actual file should be named `deployment_local.yaml` and is ignored by GIT (via .gitignore)
The `deployment_local.yaml` includes confidential information for each user, e.g. dockerhub username and AWS access keys.
Important sections:
 - `credentials` - please refer to the [AWS Setup](#awssetup) section
 - `access_token` - please refer to the [Docker setup](#dockerhubsetup)
 - `local`
   - `concord_root` - path to the vmwathena_blockchain folder on your local machine
   - `perf_test_root` - path to the local results folder
 - `runtime` - this section allows to set optional command to be run at some interval on each replica
               e.g. for instrumenting concord
    - `custom_script_path` - path to local script under `{concord_root}/concord/src/performance/deployement` foler
    - `custom_command` - the command to be executed during replicas run
                         Currently the string `CONTAINER_ID` is supported as parameter
                         and will be replaced in the runtime by actual container ID.
      `custom_command_run_interval_sec` - periodic interval to run custom command, in seconds
 - `general`
    -`num_of_replicas` - we support 4 or 7 only

### AWS setup
AWS users may have IAM account type, where the credentials provided by Amazon are in form of **account ID**,**user name**, **password** tuple, or they may use SSO (which is used in VMWare, for example, called Cloudgate) to obtain AWS access.
Current deployment code has been tested with *US-Ohio* AWS region. Keys and images in AWS are not shared between different regions so it's very important to use
- All users:
  - Login to the [AWS console](https://aws.amazon.com/console/) using your get_credentials. Choose `IAM User' when logging in.
  For the Cloudgate users see *Cloudgate users* section below for instructions on how to access AWS console
  - Pick the `US-Ohio (us-east-2)` from the dropbox in the upper right corner if it is not selected by default. The dashboard will switch to the new region view.
  - From the `Services` dropbox in the upper left corner choose `EC2`. The EC2 Management console will open
  - On the left side menu click `Key Pairs` under `Network & Security`. Key Pair Console will open
  - Click on `Create Key Pair` button
  - Provide name for the new Key Pair, e.g. `concord-perf`. Click `Create`. The Key Pair will be created and the browser will download the .PEM file to your machine.
  - Copy the .PEM file to the `~/.ssh` folder on your machine
  - Navigate to the `~/.ssh` folder in the terminal and type `chmod 400 concord-perf.pem` (assuming you chose `concord-perf` as a key name).
  - Use the path to the key file and the key file name as values for `ssh_key_path` and `ssh_key_name`, respectively, in `deploy_local.yaml`
- IAM users:
  - Login to the [AWS console](https://aws.amazon.com/console/) using your get_credentials. Choose `IAM User` when logging in.
  - From the `Services` dropbox in the upper left corner choose `IAM`. The IAM Management console will open
  - On the left menu click `Users`. User Management console will open.
  - Click the user (usually you will have only one). User Details console will open.
  - Click `Security Credentials` tab
  - Click `Create access key` button
  - Click `Show` in the popup window
  - Use `Access key ID` and `Secret access key` values as values for the `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` parameters in the `deployment_local.yaml` file. You don't need any value for `AWS_SESSION_TOKEN` parameter.
- Cloudgate users
  - Login with SSO (e.g. https://console.cloudgate.vmware.com/ui/#/login)
  - Click on `Organization Accounts`
  - Click on `Get Temporary Access`
  - Choose `PowerUser` role in the dropbox and click `Next`
  - Use values from the `AWS CLI Access` section as parameters for the `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` and `AWS_SESSION_TOKEN` in the `deployment_local.yaml' file.
  - Note that you can access AWS Console using the `AWS Console Access` link

### Docker setup
This script uses DockerHub to transfer images from local machine to the destination machines.

- Login to [DockerHub](https://hub.docker.com)
- Click on your username on the upper right corner. Choose `Account settings`
- On the left side menu click `Security`
- Click `New Access Token`
- Provide friendly name and click `Create`
- ***IMPORTANT: now it's the only place when you can copy the token. You can't access it after you exit this screen***
- Copy the Access Token and use it's value as the parameter for `access_token` under `docker` section in the `deployment_local.yaml`
- Use the user name you used to login to the DockerHub as the value for `username` under `docker` section in the `deployment_local.yaml`

### Running the deployment script
The main script is `deploy.py`. There are few dependencies that should be installed with PIP:

`pip install pyyaml boto3 paramiko`

When run with no parameters, this script performs the following steps:
- Creates or starts AWS machines (4 replicas and 1 loader)
- Builds local Concord and Loader Docker images from the **current branch code base**
- Push these images to the DockerHub
- Runs SSH sessions to the AWS machines
- Pull images from DockerHub to the machines
- Runs replicas and loader
- Collects logs and core dumps, if created, from the machines

***By default machines are not stopped after each run!!!***

The behaviour can be modified by the following CLI parameters:
- `-noBuildConcord` - don't build Concord image. The latest one will be used
- `-noBuildLoader` - don't build Loader image. The latest one will be used
- `-noPushConcord` - don't push Concord image to the DockerHub
- `-noPushLoader` - don't push Loader image to the DockerHub
- `-cleanRemoteFolder` - clean remote folder from previous runs results
- `-copyAllData` - copy RocksdDb folder to the local machine
- `-useExistingInstances` - use pre-created instances (for debug only)
- `-setupMachinesOnly` - setup AWS machines and stop (for debug only)
- `-stopMachines` - stop instances after the run
- `-noRunTest` - Don't run actual test. Will run only replicas and it allows to run loader container manually.
                ***Note: the script may hang at this point, just exit with CTRL-C***

The following set of CLI parameters control the Concord replica configuration:
- `-inMemory` - use in-memory DB instead of RocksdDb
- `-useBasicKVBC` - use v1 KVBC (no Merkle tree)

Note: Please ignore warning and error at the beginning of the run

### Performance Loader Tool
The PLT is a stand alone binary, which is containerized and runs on the separate machine. It accepts the following CLI parameters:
  - -b (INTEGER) - number of requests to launch (default: 4000000)
  - -p (1/0) - requests pre-processing on/off (default: off)
  - -k (INTEGER) - number of keys (default: 31)
  - -s (INTEGER) - single key size (default: 118)
  - -v (INTEGER) - key value size (default: 2841)
  - -d (INTEGER) - payload size (default: 15100)
  - -c (INTEGER) - concurrency level: capacity of the clients pool (default: 15)
  - -e (INTEGER) - execution time (default: 0)
  - -f (STRING) - path to the client pool configuration file
  - -w (1/0) - to use a busy-wait (1) or a regular sleep simulating execution time latency (default: 1)
  - -i (1/0) - to print (1) or not (0) request durations (default: 1)
  - -l (0,1,2,3 - off, error, info, debug) - log level (default: 1"). If log4cplus is not used, we use this file.
  - -a STRING - path to log4cplus.properties file for the loader. Default: assumes it runs within Docker container with mapped folder to /perf_loader/config

  These parameters can be changed via the `loader_cli` parameter in the `deployment_local.yaml` file

### Logger configuration
The `log4cplus.properties` file in the current folder is used for each run to configure replica's logger. Just add/modify any named loggers to have desired output in the log files.
