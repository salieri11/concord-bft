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
  export DEFAULT_SPIDER_IMAGE_TAG=1.30.0
  export DEFAULT_DAML_SDK_VERSION=1.4.0
  export DEFAULT_MARKET_FLAVOUR=sample
  export DEFAULT_LOAD_RUNNER_TRADE_TIMEOUT=180
  export DEFAULT_LOAD_RUNNER_CONCURRENCY=8
  export DEFAULT_LOAD_RUNNER_REQUESTS=333
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
  ## Set spider app metrics port to something that doesn't clash:
  SPIDER_APP_METRICS_PORT=54321
}

load_docker_images() {
  docker pull digitalasset/spider-application:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-tools:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-load-runner:${SPIDER_IMAGE_TAG}
}

start_concord () {
  start_network
  (cd docker && ./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml.yaml)
  (cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp.yml up -d)
  ## Let concord stabilize
  sleep 60
}

stop_concord () {
  ## Shut down concord
  (cd docker && docker-compose -p "docker" --project-directory . -f ../daml/gcp/docker-compose-daml-gcp.yml down)
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
  start-spider
}

stop_spider () {
  [ "$(docker ps | grep spider)" ] && docker kill spider-${SPIDER_IMAGE_TAG}
}

create_prerequisites () {
  ## Extract the necessary parties from the test data set
  allocate-ledger-party 00001 $(_get_all_sample_parties ${MARKET_FLAVOUR})

  ## Create genesis contracts
  market-setup | grep -v akka
  ## Load sample data for load-runner
  import-data-set ${MARKET_FLAVOUR} | grep -v akka
}

run_load_runner () {
  ## Open the market
  load-runner --simulation bmw.open-market
  sleep 10
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

run_test () {

  echo "Launching load runner performance tool against DAML on VMware"
  set -x

  initialize_env_variables

  load_docker_images
  start_concord

  locate_ledger_host
  import_spider_scripts

  install_dar
  start_spider
  create_prerequisites

  run_load_runner

  stop_spider
  stop_concord
}

stop_all () {
  initialize_env_variables
  stop_spider
  stop_concord
  stop_prometheus
}

print_help () {
  default_env_variables
  echo -e "Usage: ./load-runner.sh COMMAND\n"
  echo "Available commands:"
  echo "help                     Show this help text"
  echo "test                     Start load runner test"
  echo "lprom                    Launch prometheus in the background"
  echo "sprom                    Stop prometheus"
  echo "stop                     Stop all containers including prometheus"
  echo ""
  echo "Modify behavior of the test through environment variables:"
  echo "LEDGER_IMAGE      - pull a ledger docker image, default: $DEFAULT_LEDGER_IMAGE"
  echo "SPIDER_IMAGE_TAG  - test a specific spider version, default: $DEFAULT_SPIDER_IMAGE_TAG"
  echo "DAML_SDK_VERSION  - test a specific DAML SDK version, default: $DEFAULT_DAML_SDK_VERSION"
  echo "MARKET_FLAVOUR    - select CHESS data flavor (sample, cde7), default: $DEFAULT_MARKET_FLAVOUR"
  echo "LOAD_RUNNER_TRADE_TIMEOUT    - define how long load-runner should wait for asynchronous responses, default: $DEFAULT_LOAD_RUNNER_TRADE_TIMEOUT"
  echo "LOAD_RUNNER_CONCURRENCY      - define how many threads should load-runner use for communicating with the ledger, default: $DEFAULT_LOAD_RUNNER_CONCURRENCY"
  echo "LOAD_RUNNER_REQUESTS         - define how many trade registration requests should load-runner make, default: $DEFAULT_LOAD_RUNNER_REQUESTS"
}

OPERATION=${1:-"help"}

case $OPERATION in

  test)
    run_test
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
