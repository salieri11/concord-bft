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

Use [docker login](https://docs.docker.com/engine/reference/commandline/login/) command to enable access to fluentd image.

```bash
docker login -u <username>
```

## Features

### Logs 

Docker logs on the load driver VM (refer [fluent-log-insight.conf](fluent-log-insight.conf)) are collected and sent to Log Insight and uploaded to Apache server too.
Apart from that, docker logs from all committer and client nodes get collected using SCP.

### Metrics

Telegraf collects metrics (refer [telegraf.conf](telegraf.conf)) at a regular interval and sends to Wavefront.

### Progress

Load Runner sends live progress of trade requests to InfluxDB, which could be viewed using Grafana dashboard. 

### Bundle

At the end of a test, the entire bundle with logs and test report on the local file system is sent in the following format to Apache server using SCP.
The bundle is identified by blockchain id, and date & time of the test.

```bash
<blockchain-id>
|  
└───<test-date>
    │
    │─── logs
    │    │
    │    │ <IP>
    │    │ <IP>
    │    │ ...
    │
    └─── reports
         │
         │ load-runner-XXX
         │ load-runner-XXX
         │ ...
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

As fluentd docker container runs as root user, the files it creates on the host file system cannot be deleted by a non-root user.
To enable using sudo in the script, sudo password prompt needs to be disabled.

```bash
sudo visudo
# $USER ALL=(ALL) NOPASSWD:ALL
```
