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

At the end of a test, all logs and reports on the local file system are sent to Apache HTTP server using SCP.

### Notification

Slack Incoming Webhooks are used to send notifications related to start and end of a test.

### Usage

Configure [.env](.env) and execute the script.

```bash
./chess_plus.sh
```


