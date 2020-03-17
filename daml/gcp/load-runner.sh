#!/bin/bash

echo "Launching load runner performance tool against DAML on VMware"

set -x

## Define few variables
export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG:-1.23.155}
export DAML_SDK_VERSION=${DAML_SDK_VERSION:-0.13.52}

## Load docker images
docker pull digitalasset/spider-application:${SPIDER_IMAGE_TAG}
docker pull digitalasset/spider-tools:${SPIDER_IMAGE_TAG}
docker pull digitalasset/spider-load-runner:${SPIDER_IMAGE_TAG}

## Launch concord
docker network create vdaml_testing
(cd docker && pwd)
(cd docker && ./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml.yaml)
(cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp.yml up &>/dev/null &)
## Let concord stabilize
sleep 120

## Populate ledger host variable
export LEDGER_HOST=`docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' docker_daml_ledger_api1_1`

## Source the bash functions used in tests
source /dev/stdin <<< "$(docker run --rm digitalasset/spider-application:${SPIDER_IMAGE_TAG} vdaml-bootstrap)"

## Copy dar file out of spider-application container
extract-dar
## Upload dar file to ledger
upload-dar
## Stabilize the system after upload
sleep 30

## Ensure dar file is loaded
warm-package
## Extract the necessary parties from the test data set
allocate-ledger-party 00001 $(_get_all_sample_parties)
## Start spider application
start-spider

## Create genesis contracts
market-setup | grep -v akka
## Load sample data for load-runner
load-sample-data | grep -v akka

## Open the market
load-runner --simulation bmw.open-market
## Run the benchmark tool
load-runner \
	    --simulation fix-trade.standard \
	    --trade-file /home/dlt/app/spider-load-tests/data/sample/fix_ae.tsv \
	    --loop-file \
	    --trade-timeout 60s \
	    --requests 333 \
	    --concurrency 16 \
	    --spec

## Shut down concord
(cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp.yml down)
(cd docker && sudo rm -r devdata/*)
docker kill spider-${SPIDER_IMAGE_TAG}
docker network rm vdaml_testing
