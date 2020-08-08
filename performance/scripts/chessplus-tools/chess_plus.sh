#!/bin/bash

## Export variables and functions
set -ae
source .env
source .functions.sh
init_env

## Stop and remove all containers
docker stop $(docker ps -aq) || true
docker rm $(docker ps -aq) || true

## Notify
slack_msg .msg.1.json

## Bootstrap
source /dev/stdin <<<"$(docker run --rm digitalasset/spider-application:${SPIDER_IMAGE_TAG} vdaml-bootstrap | sed 's/--rm//g')"

## Start side-cars
start-sidecars
sleep 10

## Create parties for the system to initialise correctly.
allocate-ledger-party 00001

## Upload dar file to ledger
upload-dar

## Ensure dar file is loaded
warm-package

## Start spider application
start-spider
sleep 10

## Create genesis contracts
market-genesis

## Load sample data for load-runner
import-data-set ${MARKET_FLAVOUR}

## Start Grafana on port 3000
start-dashboard
sleep 10

## Open the market
load-runner --simulation bmw.open-market

# Data set file
data_file="fix_ae.tsv"
if [ "${MARKET_FLAVOUR}" = "nfr" ]; then
  data_file="20191205_Trades.zip"
fi

## Run fix-trade
load-runner \
  --simulation fix-trade.standard \
  --trade-file "/home/dlt/app/spider-load-tests/data/${MARKET_FLAVOUR}/${data_file}" \
  --loop-file \
  --trade-timeout 60s \
  --requests ${TRADE_COUNT} \
  --concurrency ${CONCURRENCY} \
  --spec

## Stop Grafana
stop-dashboard

## Stop the spider application
stop-spider

## Stop side-cars
stop-sidecars

## Remove stopped containers
docker rm $(docker ps -aq) || true

## Bundle load-runner reports
bundle_reports

## Bundle blockchain logs
bundle_logs

## Zip logs to reduce size
zip_logs

## Upload bundle to Apache server
upload_bundle

## Notify
slack_msg .msg.2.json

# Delete local data
rm spider-modules-*.dar
sudo rm -rf $BLOCKCHAIN_ID

# shellcheck disable=SC2086
