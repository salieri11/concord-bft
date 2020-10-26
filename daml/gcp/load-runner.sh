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
  export DEFAULT_SPIDER_IMAGE_TAG=1.33.91
  export DEFAULT_DAML_SDK_VERSION=1.6.0
  export DEFAULT_MARKET_FLAVOUR=sample
  export DEFAULT_LOAD_RUNNER_RATE=20
  export DEFAULT_LOAD_RUNNER_CONCURRENCY=12
  export DEFAULT_LOAD_RUNNER_REQUESTS=2000
  export DEFAULT_NETTING_SIZE=100
  export DEFAULT_USE_PREEXECUTION=true
  export DEFAULT_SIMULATION_MODE=open
}

initialize_trade_source () {
  case ${MARKET_FLAVOUR} in
    sample)
      export TRADE_SOURCE="data/sample/trades-20180912-WOW-10k.tsv.gzip"
      ;;
    nfr)
      export TRADE_SOURCE="data/nfr/20191205_Trades.tsv.gzip"
      ;;
    storage)
      export TRADE_SOURCE="data/storage/20200810_Trades.tsv.gzip"
      ;;
  esac
}

initialize_env_variables () {
  default_env_variables
  ## Define image version variables
  export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG:-$DEFAULT_SPIDER_IMAGE_TAG}
  export DAML_SDK_VERSION=${DAML_SDK_VERSION:-$DEFAULT_DAML_SDK_VERSION}
  export MARKET_FLAVOUR=${MARKET_FLAVOUR:-$DEFAULT_MARKET_FLAVOUR}
  export LOAD_RUNNER_RATE=${LOAD_RUNNER_RATE:-$DEFAULT_LOAD_RUNNER_RATE}
  export LOAD_RUNNER_CONCURRENCY=${LOAD_RUNNER_CONCURRENCY:-$DEFAULT_LOAD_RUNNER_CONCURRENCY}
  export LOAD_RUNNER_REQUESTS=${LOAD_RUNNER_REQUESTS:-$DEFAULT_LOAD_RUNNER_REQUESTS}
  export NETTING_SIZE=${NETTING_SIZE:-$DEFAULT_NETTING_SIZE}
  export USE_PREEXECUTION=${USE_PREEXECUTION:-$DEFAULT_USE_PREEXECUTION}
  export SIMULATION_MODE=${SIMULATION_MODE:-$DEFAULT_SIMULATION_MODE}
  export DAY_T1=2018-09-13
  export DAY_T2=2018-09-14
  ## Set spider app metrics port to something that doesn't clash:
  SPIDER_APP_METRICS_PORT=54321
  if [[ "$USE_PREEXECUTION" == "true" ]]; then
    export TEST_SUFFIX="-preexecution"
  fi
  initialize_trade_source
}

load_docker_images() {
  docker pull digitalasset/spider-application:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-tools:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-load-runner:${SPIDER_IMAGE_TAG}
  docker pull digitalasset/spider-compose:${SPIDER_IMAGE_TAG}
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

import_spider_scripts () {
  ## Source the bash functions used in tests
  source /dev/stdin <<< "$(docker run --rm --entrypoint /render_bootstrap ${IMAGE_REGISTRY:-digitalasset}/spider-compose:${SPIDER_IMAGE_TAG})"
}

set_docker_network () {
  _manage_override_value DOCKER_NETWORK_ENABLED true
  _manage_override_value DOCKER_NETWORK_NAME vdaml_testing
}

locate_ledger_host () {
  ## Populate ledger host variable
  _manage_override_value LEDGER_HOST "$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' docker_daml_ledger_api1_1)"
  _manage_override_value LEDGER_PORT 6865
}

install_dar () {
  ## Copy dar file out of spider-application container
  extract_dar
  ## Upload dar file to ledger
  upload_dar
  ## Stabilize the system after upload
  sleep 30
  ## Ensure dar file is loaded
  warm_package
}

start_spider () {
  ## Start spider application
  spider up
}

stop_spider () {
  spider down
  docker ps -a --filter "name=spider_kafka" -q | xargs docker rm -f
}

create_prerequisites () {
  ## Create genesis contracts
  spider_market_genesis
  ## Load sample data for load-runner
  spider_import_market_set ${MARKET_FLAVOUR}
}

run_simulation () {

  if [[ "$SIMULATION_MODE"=="open" ]]; then
    SIMULATION_NAME=$1
    MODE_SUFFIX="-o arrival.rate=${LOAD_RUNNER_RATE}"
  else
    SIMULATION_NAME=$2
    MODE_SUFFIX="-o concurrent.ars=${LOAD_RUNNER_CONCURRENCY}"
  fi

  DAYS_SPEC=${@:3}
  DURATION=$(( ${LOAD_RUNNER_REQUESTS} / ${LOAD_RUNNER_RATE} ))

  kafka_ledger_bridge_up ${LOAD_RUNNER_CONCURRENCY}
  gatling_simulations \
    -s ${SIMULATION_NAME} \
    -o duration="${DURATION} seconds" \
    -o trades.source=${TRADE_SOURCE} \
    ${MODE_SUFFIX} \
    ${DAYS_SPEC}

}

process_results () {
  REQUEST_TYPE=${1:-"send trades (pick-to-AR)"}
  HEADER_ECHO="===================== RESULTS: ${REQUEST_TYPE} ====================="
  echo ${HEADER_ECHO}
  TARGET="$(find results -name "*-202*" -and -type d | sort -r | head -n 1)"
  if [[ -z "${TARGET}" ]]; then
    MACHINE="$(hostname)"
    csvjson "${TARGET}/summary.csv" | \
      yq --yaml-output \
        --arg machine ${MACHINE} \
        --arg market_flavour ${MARKET_FLAVOUR} \
        --arg request_type "${REQUEST_TYPE}" \
        '. | map(select(.request == $request_type)) | first | {
          "Machine": $machine,
          "Ledger": "daml-on-vmware-concord",
          "Market flavour": $market_flavour,
          "Trades/second": .rps,
          "Trades": .count,
          "Duration": .duration,
          "Latency": {
            "min": .min,
            "mean": .mean,
            "avg": .avg,
            "p90": .p90,
            "p95": .p95,
            "p99": .p99,
            "p999": .p999,
            "max": .max,
            "stddev": .stddev,
          }
        }'
  else
    echo "no results were found"
  fi
  echo ${HEADER_ECHO} | sed -e 's/./=/g'
}

run_test () {

  echo "Launching load runner performance tool against DAML on VMware"

  load_docker_images
  start_concord

  import_spider_scripts
  set_docker_network
  locate_ledger_host

  install_dar
  create_prerequisites
  start_spider

  run_simulation $@

  process_results

  stop_spider
  stop_concord
}

stop_all () {
  import_spider_scripts
  set_docker_network
  stop_spider
  kafka_ledger_bridge_down
  stop_concord
  stop_prometheus
}

print_help () {
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
  echo "SIMULATION_MODE   - select the simulation mode (open, closed), default: $DEFAULT_SIMULATION_MODE"
  echo "LOAD_RUNNER_RATE  - select the nominal trade rate, default: $DEFAULT_LOAD_RUNNER_RATE"
  echo "LOAD_RUNNER_CONCURRENCY      - define how many threads should load-runner use for communicating with the ledger, default: $DEFAULT_LOAD_RUNNER_CONCURRENCY"
  echo "LOAD_RUNNER_REQUESTS         - define how many trade registration requests should load-runner make, default: $DEFAULT_LOAD_RUNNER_REQUESTS"
}

initialize_env_variables

OPERATION=${1:-"help"}

case $OPERATION in

  trade)
    run_test \
      "TradesAtFixedRate" \
      "ClosedModelTrades"
    ;;

  net)
    run_test \
      "SingleDayNetting" \
      "ClosedModelSingleDayNetting" \
      -o date.day2=${DAY_T1}
    ;;

  settle)
    run_test \
      "SingleDaySettlementInclNetting" \
      "ClosedModelSingleDaySettlementInclNetting" \
      -o date.day2=${DAY_T1} \
      -o date.day3=${DAY_T2}
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
