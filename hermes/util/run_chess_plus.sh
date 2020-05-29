# Shell script version to run chess plus
#!/bin/bash

EXIT_STATUS=1
MARKET_FLAVOUR=sample
LEDGER_HOST=localhost
LEDGER_PORT=6865
CONCURRENCY=3

TMP=/tmp
TIME_STAMP=`date +%m%d%Y_%H%M%S`
WORK_DIR="${TMP}/chess_plus_on_daml_${TIME_STAMP}"

info() {
    echo `date`: INFO: "${1}"
}

error() {
    echo `date`: ERROR: "${1}"
}

check_usage() {
    if [ -z "$LEDGER_HOST" -o -z "$DAML_SDK_VERSION" -o -z "$SPIDER_IMAGE_TAG" -o -z "$MARKET_FLAVOUR" ]
    then
        error "Missing parameters"
        cat << EOF
Usage: $0 <OPTIONS>
          --ledgerHost <DAML ledger host>
          --spiderImageTag <Spider image tag>
          --marketFlavor <Market flavor>
          --damlSDKVersion <DAML SDK version>
          --concurrency <Concurrency throttles the number of in-flight trades between the submission and the response of the first hop>
EOF

        exit 1
    fi
}

load_spider_application() {
    source /dev/stdin <<< "$(docker run --rm digitalasset/spider-application:${SPIDER_IMAGE_TAG} vdaml-bootstrap)"
}

execute_command() {
    INITIALIZE_ONCE=""
    VALIDATE_STRING=""
    while [ "$1" != "" ] ; do
        case $1 in
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
        error "execute_command(): Missing parameter '--comandToRun <command>'"
        exit 1
    else
        CMD_AS_FILE_NAME=`echo "${COMMAND_TO_RUN}" | tr '/' '_'`
        LOG_FILE="${WORK_DIR}/${CMD_AS_FILE_NAME}".log
        IS_CMD_TO_BE_INITIALIZED_ONCE_FILE="${WORK_DIR}/${CMD_AS_FILE_NAME}.${LEDGER_HOST}"

        info "****************************************"
        info "Command: ${COMMAND_TO_RUN}..."
        if [ -f "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}" ]
        then
            info "Tracker file ${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE} found"
            info "Skipping 2nd attempt of running command '${COMMAND_TO_RUN}' on ledger host: ${LEDGER_HOST}"
        else
            info "Running ${COMMAND_TO_RUN}, output to ${LOG_FILE}"
            eval "${COMMAND_TO_RUN}" > "${LOG_FILE}"
            RC="$?"
            if [ ! -z "${INITIALIZE_ONCE}" ]
            then
                touch "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}"
            fi
        fi

        info ""

        if [ "${RC}" == "0" -o -f "${IS_CMD_TO_BE_INITIALIZED_ONCE_FILE}" ]
        then
            if [ ! -z "${VALIDATE_STRING}" ]
            then
                keyword_matched=`cat "${LOG_FILE}" | grep "${VALIDATE_STRING}"`
                if [ ! -z "${keyword_matched}" ]
                then
                    info "**** Command ${COMMAND_TO_RUN} execution/validation completed sucessfully"
                else
                    error "**** Command ${COMMAND_TO_RUN} execution failed"
                    trap_ctrlc
                fi
            else
                info "**** Command execution completed sucessfully"
            fi
        else
            error "**** Command execution failed"
            trap_ctrlc
        fi
        info ""
    fi
}

run_chess_plus_test() {
    info "************************************************************"
    info "Ledger API Host: $LEDGER_HOST"
    info ""

    load_spider_application
    execute_command --commandToRun vdaml_help --validateString "Commands exposed by Spider vDAML"
    execute_command --commandToRun upload-dar --validateString "DAR upload succeeded" --initializeOnce
    execute_command --commandToRun warm-package --validateString "{" --initializeOnce
    execute_command --commandToRun "echo ${DAML_SDK_IMAGE}" --validateString "${DAML_SDK_IMAGE}"
    execute_command --commandToRun "allocate-ledger-party 00000 00001" --validateString "Allocated '00001' for '00001' at ${LEDGER_HOST}:${LEDGER_PORT}" --initializeOnce
    execute_command --commandToRun start-spider --validateString "returned: 0"
    execute_command --commandToRun market-genesis --validateString "Create: 1" --initializeOnce
    execute_command --commandToRun "import-data-set ${MARKET_FLAVOUR}" --validateString "Failure: 0" --initializeOnce
    execute_command --commandToRun "load-runner --simulation bmw.open-market" --validateString "failed[' ']*0"
    execute_command --commandToRun "load-runner --simulation fix-trade.standard --trade-file /home/dlt/app/spider-load-tests/data/${MARKET_FLAVOUR}/fix_ae.tsv --loop-file --trade-timeout 60s --requests 1 --concurrency ${CONCURRENCY} --spec" --validateString "failed[' ']*0"
    execute_command --commandToRun stop-spider
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
        "--setupMarketOnce")
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

    esac
    shift
done

check_usage
if  [ ! -d "${WORK_DIR}" ]
then
    mkdir -p "${WORK_DIR}"
fi
cd "${WORK_DIR}"

CHESS_PLUS_RUN_LOG_FILE="${WORK_DIR}/run.log"
export SPIDER_IMAGE_TAG="$SPIDER_IMAGE_TAG"
export DAML_SDK_VERSION="$DAML_SDK_VERSION"
export LEDGER_HOST="$LEDGER_HOST"
export LEDGER_PORT="$LEDGER_PORT"
export MARKET_FLAVOUR="$MARKET_FLAVOUR"
export CONCURRENCY="$CONCURRENCY"

run_chess_plus_test 2>&1 > "${CHESS_PLUS_RUN_LOG_FILE}"

info "Exit status for this run: $EXIT_STATUS"
exit "$EXIT_STATUS"
