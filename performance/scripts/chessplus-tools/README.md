# CHESS+ Tools

## Overview

These tools add observability and monitoring to CHESS+ (Spider) and Load Runner on load driver VM.

## Prerequisites

Install [sshpass](https://linux.die.net/man/1/sshpass) to enable non-interactive SSH login.

```bash
sudo apt install sshpass
```

Install [jq](https://stedolan.github.io/jq/) to convert JSON logs to plain text.

```bash
sudo apt install jq
```

[Log in to Docker registry](https://docs.docker.com/engine/reference/commandline/login/) to access blockchain fluentd image.

```bash
docker login -u <username>
```

## Features

### Logs 

fluentd collects logs from all the relevant docker containers on the load driver VM. The target for these logs is either Log Insight (refer [fluent-log-insight.conf](fluent-log-insight.conf)) or local file system (refer [fluent-log-file.conf](fluent-log-file.conf)).
Apart from that, the docker logs from all committer and client nodes get collected using SCP.

### Metrics

Telegraf collects metrics (refer [telegraf.conf](telegraf.conf)) at a regular interval and sends to Wavefront.

### Progress

Load Runner sends live progress of trade requests to InfluxDB, which could be viewed using Grafana dashboard. 

### Test Result

At the end of a test, the entire bundle of result on the local file system is sent to Apache HTTP server using SCP.
Following is the format of the bundle.

```bash
<blockchain-id>
|  
│
└───<test-date>
    │   file011.txt
    │   file012.txt
    │
    └───logs
        │
        | <IP 1>
        │ <IP 2>
        │ ...
        |
    -───reports
        |
        | load-runner-XXX
        | load-runner-XXX    
```

### Notification

Slack Incoming Webhooks are used to send notifications related to start and end of a test.

## Usage

Configure [.env](.env) and execute the script.

```bash
./chess_plus.sh
```

## Limitations and Workaround

### Delete directory

Since fluentd docker container runs as root user, the files it creates on host file system are owned by the root and therefore not deleted as part of the script.

```bash
sudo rm -rf <blockchain-id>
```

### Remove containers

The script takes care of removing all the docker containers, but in case it is terminated abruptly, the containers need to be removed manually.

```bash
docker stop $(docker ps -aq)
docker rm $(docker ps -aq)
```
