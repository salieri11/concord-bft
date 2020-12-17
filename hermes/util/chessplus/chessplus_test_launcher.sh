# Shell script version to run chess plus
#!/bin/bash

EXIT_STATUS=1
LEDGER_HOST=localhost
LEDGER_PORT=6865
MARKET_FLAVOR_NFR=nfr
DAY2="2019-12-06"
DAY3="2019-12-09"

TMP=/tmp
TIME_STAMP=`date +%Y%m%d_%H%M%S`

info() {
    echo `date`: INFO: "${1}"
}

error() {
    echo `date`: ERROR: "${1}"
}

check_usage() {
    if [ -z "${LEDGER_HOST}" -o -z "${DAML_SDK_VERSION}" -o -z "${SPIDER_IMAGE_TAG}" -o -z "${MARKET_FLAVOUR}" -o -z "${CONCURRENCY}" -o -z "${WORK_DIR}" -o -z "${ARRIVAL_RATE}" -o -z "${DURATION}" -o -z "${UNIT}" -o -z "${OPERATION}" -o -z "${MAX_DURATION}" ]
    then
        error "Missing parameters"
        cat << EOF
Usage: $0 <OPTIONS>
          --ledgerHost <DAML ledger host>
          --spiderImageTag <Spider image tag>
          --marketFlavor <Market flavor>
          --damlSDKVersion <DAML SDK version>
          --concurrency <Concurrency throttles the number of in-flight trades between the submission and the response of the first hop>
          --resultsDir <Results/log directory path>
          --chessplus_trades_arrival_rate <Number of fixed trades per second>
          --chessplus_run_duration <Duration for running simulation>
          --chessplus_max_run_duration <Max duration for running simulation>
          --chessplus_time_units <Time unit for running simulation>
          --chessplus_operation <Simulation to generate stream of trades>
EOF

        exit 1
    fi
}

load_spider_application() {
    source /dev/stdin <<< "$(docker run --rm digitalasset/spider-tools:${SPIDER_IMAGE_TAG} bootstrap)"
}

create_initialize_once_file() {
    if [ "${INITIALIZE_ONCE}" ] ; then
        touch "$1"
    fi
}

command_execution_failure() {
    if [ "${MAX_RETRIES}" -ne "1" ] && [ "${ATTEMPT}" -ne "${MAX_RETRIES}" ]; then
        error "**** Command $1 execution/validation failed at attempt: ${ATTEMPT}, retrying... "
    else
        error "**** Command $1 execution/validation failed..."
        trap_ctrlc
    fi
}

execute_command() {
    INITIALIZE_ONCE=""
    VALIDATE_STRING=""
    MAX_RETRIES=1
    ATTEMPT=0
    while [ "$1" != "" ] ; do
        case $1 in
            "--maxRetries")
                shift
                MAX_RETRIES="$1"
                ;;
            "--commandToRun")
                shift
                COMMAND_TO_RUN="$1"
                ;;
            "--validateString")
                shift
                VALIDATE_STRING="$1"
                ;;
            "--initializeOnce")
                INITIALIZE_ONCE="$1"
                ;;
        esac
        shift
    done

    if [ -z "${COMMAND_TO_RUN}" ]
    then
        error "execute_command(): Missing parameter '--commandToRun <command>'"
        exit 1
    else
        CMD_AS_FILE_NAME=`echo "${COMMAND_TO_RUN}" | tr '/' '_'`
        if [[ "${COMMAND_TO_RUN}" == *"gatling_simulations"* ]]
        then
            CMD_AS_FILE_NAME="gatling_simulations"
        fi
        LOG_FILE="${WORK_SUBDIR}/${CMD_AS_FILE_NAME}".log
        IS_CMD_TO_BE_INITIALIZED_ONCE_FILE="${WORK_DIR}/../${CMD_AS_FILE_NAME}.${LEDGER_HOST}"

        info "****************************************"
        info "Command: ${COMMAND_TO_RUN}..."
        if [ -f "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}" ]
        then
            info "Tracker file ${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE} found"
            info "Skipping 2nd attempt of running command '${COMMAND_TO_RUN}' on ledger host: ${LEDGER_HOST}"
        else
            while [ "${ATTEMPT}" -lt "${MAX_RETRIES}" ]; do
                ATTEMPT=`expr ${ATTEMPT} + 1`
                info "Attempt: ${ATTEMPT}, Running ${COMMAND_TO_RUN}, output to ${LOG_FILE}"
                eval "${COMMAND_TO_RUN}" > "${LOG_FILE}"
                RC="$?"

                info ""

                if [ "${RC}" == "0" ]
                then
                    if [ ! -z "${VALIDATE_STRING}" ]
                    then
                        keyword_matched=`cat "${LOG_FILE}" | grep "${VALIDATE_STRING}"`
                        if [ ! -z "${keyword_matched}" ]
                        then
                            info "**** Command ${COMMAND_TO_RUN} execution/validation completed successfully"
                            create_initialize_once_file "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}" "${INITIALIZE_ONCE}"
                            break
                        else
                            command_execution_failure "${COMMAND_TO_RUN}"
                        fi
                    else
                        info "**** Command execution completed successfully"
                        create_initialize_once_file "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}" "${INITIALIZE_ONCE}"
                        break
                    fi
                else
                    command_execution_failure "${COMMAND_TO_RUN}"
                fi
            done
        fi
    fi
}

run_chess_plus_test() {
    info "************************************************************"
    info "Ledger API Host: $LEDGER_HOST"
    info ""

    load_spider_application
    execute_command --commandToRun "_manage_override_value LEDGER_HOST ${LEDGER_HOST}" --validateString "done"
    execute_command --commandToRun "_manage_override_value LEDGER_PORT 6865" --validateString "done"
    execute_command --commandToRun "_manage_override_value IMPORTER_OPTS -DbatchAutoResize=true -Dparallelism=3 -Dbatch=20" --validateString "done"
    execute_command --commandToRun "dashboard_up" --validateString "done"
    execute_command --commandToRun "upload_dar --timeout 250" --initializeOnce --maxRetries 5 --validateString "DAR upload succeeded."
    execute_command --commandToRun "echo ${DAML_SDK_IMAGE}" --validateString "${DAML_SDK_IMAGE}"
    execute_command --commandToRun "allocate_ledger_party pkgwrmr" --initializeOnce --validateString "Success"
    execute_command --commandToRun "warm_package pkgwrmr" --validateString "{" --initializeOnce
    execute_command --commandToRun "spider_market_genesis" --validateString "Create: 1" --initializeOnce
    execute_command --commandToRun "_manage_override_value SPIDER_OPTS --disable-auto-accept-proposals"
    execute_command --commandToRun "spider up"
    execute_command --commandToRun "spider_proposals_off" --validateString "OK" --maxRetries 5
    execute_command --commandToRun "spider_import_market_set ${MARKET_FLAVOUR}" --validateString "Failure: 0" --initializeOnce
    execute_command --commandToRun "set_spider_config_overrides --nettingBatchSize ${nettingsize:-10} --batchSettlementBatchSize ${settlementsize:-10} --batchSettlementSkipPrepaymentAuth true"
    execute_command --commandToRun "kafka_ledger_bridge_up ${CONCURRENCY}" --validateString "bmw-egress"
    execute_command --commandToRun "spider_proposals_on" --validateString "OK" --maxRetries 5
    # run simulation:
    if [ "${MARKET_FLAVOUR}" = "nfr" ]
    then
        if [ "${OPERATION}" = "SingleDaySettlementInclNetting" ]
        then
            execute_command --commandToRun "gatling_simulations -s ${OPERATION} -o arrival.rate=${ARRIVAL_RATE} -o sim.max.duration=\"${MAX_DURATION} ${UNIT}\" -o date.day2=${DAY2} -o date.day3=${DAY3} -o duration=\"${DURATION} ${UNIT}\"  -o sla.bucket=\"postgres\"  -o trades.source=\"${TRADE_FILE}\"" --validateString "Simulation SingleDaySettlementInclNetting started"
        else
            execute_command --commandToRun "gatling_simulations -s ${OPERATION} -o arrival.rate=${ARRIVAL_RATE} -o sim.max.duration=\"${MAX_DURATION} ${UNIT}\" -o duration=\"${DURATION} ${UNIT}\" -o sla.bucket=\"postgres\" -o trades.source=\"${TRADE_FILE}\"" --validateString "Simulation TradesAtFixedRate started"
        fi
    else
        execute_command --commandToRun "gatling_simulations -s ${OPERATION} -o arrival.rate=${ARRIVAL_RATE} -o duration=\"${DURATION} ${UNIT}\"" --validateString "Simulation TradesAtFixedRate started"
    fi
    execute_command --commandToRun "kafka_ledger_bridge_down" --validateString "Going to remove spider_kafka-zookeeper"
    execute_command --commandToRun "spider down" --validateString "Going to remove spider_application"
    execute_command --commandToRun "dashboard_down" --validateString "Going to remove spider_"
    EXIT_STATUS=0
}

trap_ctrlc() {
    info ""
    info "Stopping spider..."
    docker stop $(docker ps -a | grep 'digitalasset' | awk '{print $1}') > /dev/null 2>&1
    docker rm $(docker ps -a | grep 'digitalasset' | awk '{print $1}') > /dev/null 2>&1
    exit 1
}

trap "trap_ctrlc" 2

while [ "$1" != "" ] ; do
    case $1 in
        "--ledgerHost")
            shift
            LEDGER_HOST="$1"
            ;;
        "--spiderImageTag")
            shift
            SPIDER_IMAGE_TAG="$1"
            ;;
        "--damlSDKVersion")
            shift
            DAML_SDK_VERSION="$1"
            ;;
        "--marketFlavor")
            shift
            MARKET_FLAVOUR="$1"
            ;;
        "--concurrency")
            shift
            CONCURRENCY="$1"
            ;;
        "--resultsDir")
            shift
            WORK_DIR="$1"
            ;;
        "--chessplusTradesArrivalRate")
            shift
            ARRIVAL_RATE="$1"
            ;;
        "--chessplusRunDuration")
            shift
            DURATION="$1"
            ;;
        "--chessplusRunMaxDuration")
            shift
            MAX_DURATION="$1"
            ;;
        "--chessplusTimeUnits")
            shift
            UNIT="$1"
            ;;
        "--chessplusOperation")
            shift
            OPERATION="$1"
            ;;
        "--logLevel")
            shift
            LOG_LEVEL="$1"
            ;;
    esac
    shift
done

check_usage
if [ "$LOG_LEVEL" == "10" ] # DEBUG
then
    set -x
fi

if  [ ! -d "${WORK_DIR}" ]
then
    mkdir -p "${WORK_DIR}"
fi

WORK_SUBDIR="${WORK_DIR}/${TIME_STAMP}_${LEDGER_HOST}"
if  [ ! -d "${WORK_SUBDIR}" ]
then
    mkdir -p "${WORK_SUBDIR}"
fi
cd "${WORK_SUBDIR}"

TRADE_FILE="data/sample/trades-20180912-WOW-10k.tsv.gzip"
CHESS_PLUS_RUN_LOG_FILE="${WORK_SUBDIR}/run.log"
export SPIDER_IMAGE_TAG="$SPIDER_IMAGE_TAG"
export DAML_SDK_VERSION="$DAML_SDK_VERSION"
export LEDGER_HOST="$LEDGER_HOST"
export LEDGER_PORT="$LEDGER_PORT"
export MARKET_FLAVOUR="$MARKET_FLAVOUR"
export CONCURRENCY="$CONCURRENCY"
export MIN_LEDGER_TIME_REL=0

# Increasing memory for spider container
export LR_JAVA_TOOL_OPTIONS='-Xmx4g'
export SPIDER_XMS=8g
export SPIDER_XMX=12g

if [ "${MARKET_FLAVOUR}" == "${MARKET_FLAVOR_NFR}" ]
then
    TRADE_FILE="data/nfr-replicated/20191205_Trades_10M.tsv.gzip"
fi

run_chess_plus_test 2>&1 > "${CHESS_PLUS_RUN_LOG_FILE}"

info "Exit status for this run: $EXIT_STATUS"
exit "$EXIT_STATUS"
