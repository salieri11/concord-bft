#!/bin/bash
# This script builds all the required images to run daml-ledger-api

PROJECT_DIR=$(python -c 'import os;from pathlib import Path;print(Path(os.path.abspath(os.getcwd())).parents[2])')

docker build $PROJECT_DIR -f DockerfileExternal -t client-pool-lib:latest
docker build $PROJECT_DIR -f $PROJECT_DIR/thin-replica-client/Dockerfile -t trc-lib:latest
docker build $PROJECT_DIR -f DockerfileCombined -t participant-lib:latest
docker build $PROJECT_DIR -f $PROJECT_DIR/daml/DockerfileLedgerApi -t daml-ledger-api:latest
