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

initialize_env_variables () {
  ## Define image version variables
  export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG:-1.23.155}
  export DAML_SDK_VERSION=${DAML_SDK_VERSION:-0.13.54}
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
  sleep 120
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
  docker kill spider-${SPIDER_IMAGE_TAG}
}

create_prerequisites () {
  ## Extract the necessary parties from the test data set
  allocate-ledger-party 00001 $(_get_all_sample_parties)

  ## Create genesis contracts
  market-setup | grep -v akka
  ## Load sample data for load-runner
  load-sample-data | grep -v akka
}

run_load_runner () {
  ## Open the market
  load-runner --simulation bmw.open-market
  ## Run the benchmark tool
  load-runner \
        --simulation fix-trade.standard \
        --trade-file /home/dlt/app/spider-load-tests/data/sample/fix_ae.tsv \
        --loop-file \
        --trade-timeout 60s \
        --requests 333 \
        --concurrency 12 \
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
  echo -e "Usage: ./load-runner.sh COMMAND\n"
  echo "Available commands:"
  echo "help                     Show this help text"
  echo "test                     Start load runner test"
  echo "lprom                    Launch prometheus in the background"
  echo "sprom                    Stop prometheus"
  echo "stop                     Stop all containers including prometheus"
  echo ""
  echo "To test specific spider version, set the SPIDER_IMAGE_TAG environment variable."
  echo "To test a specific DAML SDK version, set DAML_SDK_VERSION variable."
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
