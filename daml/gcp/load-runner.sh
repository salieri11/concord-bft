#!/bin/bash

start_network () {
  [ ! "$(docker network list | grep vdaml_testing)" ] && docker network create vdaml_testing
}

stop_network () {
  [ "$(docker network list | grep vdaml_testing)" ] && docker network rm vdaml_testing
}

start_prometheus () {
  start_network
  (cd daml/gcp && docker-compose -p "docker" --project-directory . -f docker-compose-daml-prometheus.yml up -d)
}

stop_prometheus () {
  (cd daml/gcp && docker-compose -p "docker" --project-directory . -f docker-compose-daml-prometheus.yml down)
  stop_network
}

default_env_variables () {
  export DEFAULT_SPIDER_IMAGE_TAG=1.30.477
  export DEFAULT_DAML_SDK_VERSION=1.4.0
  export DEFAULT_MARKET_FLAVOUR=sample
  export DEFAULT_LOAD_RUNNER_TRADE_TIMEOUT=30
  export DEFAULT_LOAD_RUNNER_CONCURRENCY=12
  export DEFAULT_LOAD_RUNNER_REQUESTS=2000
  export DEFAULT_NETTING_SIZE=100
  export DEFAULT_USE_PREEXECUTION=false
}

initialize_env_variables () {
  default_env_variables
  ## Define image version variables
  export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG:-$DEFAULT_SPIDER_IMAGE_TAG}
  export DAML_SDK_VERSION=${DAML_SDK_VERSION:-$DEFAULT_DAML_SDK_VERSION}
  export MARKET_FLAVOUR=${MARKET_FLAVOUR:-$DEFAULT_MARKET_FLAVOUR}
  export LOAD_RUNNER_TRADE_TIMEOUT=${LOAD_RUNNER_TRADE_TIMEOUT:-$DEFAULT_LOAD_RUNNER_TRADE_TIMEOUT}
  export LOAD_RUNNER_CONCURRENCY=${LOAD_RUNNER_CONCURRENCY:-$DEFAULT_LOAD_RUNNER_CONCURRENCY}
  export LOAD_RUNNER_REQUESTS=${LOAD_RUNNER_REQUESTS:-$DEFAULT_LOAD_RUNNER_REQUESTS}
  export NETTING_SIZE=${NETTING_SIZE:-$DEFAULT_NETTING_SIZE}
  export USE_PREEXECUTION=${USE_PREEXECUTION:-$DEFAULT_USE_PREEXECUTION}
  export DAY_T1=2018-09-13
  export DAY_T2=2018-09-14
  ## Set spider app metrics port to something that doesn't clash:
  SPIDER_APP_METRICS_PORT=54321
  if [[ "$USE_PREEXECUTION" == "true" ]]; then
    export TEST_SUFFIX="-preexecution"
  fi
}

load_docker_images() {
  docker pull digitalasset/spider-application:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-tools:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-load-runner:${SPIDER_IMAGE_TAG}
}

start_concord () {
  start_network
  (cd docker && ./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml${TEST_SUFFIX}.yaml)
  (cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp${TEST_SUFFIX}.yml up -d)
  ## Let concord stabilize
  sleep 30
}

stop_concord () {
  ## Shut down concord
  (cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp${TEST_SUFFIX}.yml down)
  (cd docker && sudo rm -r devdata/*)
  stop_network
}

locate_ledger_host () {
  ## Populate ledger host variable
  export LEDGER_HOST=`docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' docker_daml_ledger_api1_1`
}

import_spider_scripts () {
  ## Source the bash functions used in tests
  source /dev/stdin <<< "$(docker run --rm digitalasset/spider-application:${SPIDER_IMAGE_TAG} vdaml-bootstrap)"
}

install_dar () {
  ## Copy dar file out of spider-application container
  extract-dar
  ## Upload dar file to ledger
  upload-dar
  ## Stabilize the system after upload
  sleep 30
  ## Ensure dar file is loaded
  warm-package
}

start_spider () {
  ## Start spider application
  export SPIDER_OPTS="--disable-auto-accept-proposals"
  start-spider
}

stop_spider () {
  stop-spider
}

create_prerequisites () {
  ## Create genesis contracts
  market-genesis

  ## Load sample data for load-runner
  spider_proposals_off
  import-market-set ${MARKET_FLAVOUR}
  spider_proposals_on
}

trade_registration () {
  ## Open the market
  load-runner --simulation bmw.open-market
  ## Run the benchmark tool
  load-runner \
        --simulation fix-trade.standard \
        --trade-file /home/dlt/app/spider-load-tests/data/${MARKET_FLAVOUR}/fix_ae.tsv \
        --loop-file \
        --trade-timeout ${LOAD_RUNNER_TRADE_TIMEOUT}s \
        --requests ${LOAD_RUNNER_REQUESTS} \
        --concurrency ${LOAD_RUNNER_CONCURRENCY} \
        --spec
}

netting () {
  load-runner  --simulation bmw.close-market
  load-runner  --simulation bmw.day-roll --date ${DAY_T1}
  set-spider-batch-sizes ${NETTING_SIZE} ${NETTING_SIZE}
  load-runner  --simulation bmw.begin-netting --bmw-timeout 4hour --concurrency 4
}

settlement () {
  load-runner  --simulation bmw.day-roll --date  ${DAY_T2}
  load-runner  --simulation bmw.settlement  --bmw-timeout 30min --concurrency 4
}

execute_all () {
  while [[ $# -gt 0 ]]; do ($1); shift; done
}

run_test () {

  echo "Launching load runner performance tool against DAML on VMware"
  #set -x

  initialize_env_variables

  load_docker_images
  start_concord

  locate_ledger_host
  import_spider_scripts

  install_dar
  start_spider
  create_prerequisites

  execute_all $@

  stop_spider
  stop_concord
}

stop_all () {
  initialize_env_variables
  import_spider_scripts
  stop_spider
  stop_concord
  stop_prometheus
}

print_help () {
  default_env_variables
  echo -e "Usage: ./load-runner.sh COMMAND\n"
  echo "Available commands:"
  echo "help                     Show this help text"
  echo "trade                    Start load runner trade registration test"
  echo "net                      Start load runner netting test"
  echo "settle                   Start load runner settlement test"
  echo "lprom                    Launch prometheus in the background"
  echo "sprom                    Stop prometheus"
  echo "stop                     Stop all containers including prometheus"
  echo ""
  echo "Modify behavior of the test through environment variables:"
  echo "LEDGER_IMAGE      - pull a ledger docker image, default: $DEFAULT_LEDGER_IMAGE"
  echo "SPIDER_IMAGE_TAG  - test a specific spider version, default: $DEFAULT_SPIDER_IMAGE_TAG"
  echo "DAML_SDK_VERSION  - test a specific DAML SDK version, default: $DEFAULT_DAML_SDK_VERSION"
  echo "MARKET_FLAVOUR    - select CHESS data flavor (sample, cde7), default: $DEFAULT_MARKET_FLAVOUR"
  echo "USE_PREEXECUTION  - set to true to turn on the preexecution, default: $DEFAULT_USE_PREEXECUTION"
  echo "LOAD_RUNNER_TRADE_TIMEOUT    - define how long load-runner should wait for asynchronous responses, default: $DEFAULT_LOAD_RUNNER_TRADE_TIMEOUT"
  echo "LOAD_RUNNER_CONCURRENCY      - define how many threads should load-runner use for communicating with the ledger, default: $DEFAULT_LOAD_RUNNER_CONCURRENCY"
  echo "LOAD_RUNNER_REQUESTS         - define how many trade registration requests should load-runner make, default: $DEFAULT_LOAD_RUNNER_REQUESTS"
}

OPERATION=${1:-"help"}

case $OPERATION in

  trade)
    run_test trade_registration
    ;;

  net)
    run_test trade_registration netting
    ;;

  settle)
    run_test trade_registration netting settlement
    ;;

  lprom)
    start_prometheus
    ;;

  sprom)
    stop_prometheus
    ;;

  stop)
    stop_all
    ;;

  help)
    print_help
    ;;

esac
