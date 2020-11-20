# Shell script version to run DLR
#!/bin/bash

TIME_STAMP=`date +%Y%m%d_%H%M%S`
LEDGER_PORT=6865
JSON_API_HOST=127.0.0.1
JSON_API_PORT=8081
JSON_API_MSG_SIZE=72135460 #altered to pass execution
LEDGER_PORT=6865
REPO_VERSION=0.0.2

info() {
    echo `date`: INFO: "${1}"
}

error() {
    echo `date`: ERROR: "${1}"
}

check_usage() {
    if [ -z "$LEDGER_HOST" -o -z "$NO_OF_AGREEMENTS" -o -z "$LOAD_BATCH_SIZE" -o -z "$NO_OF_VUSER" -o -z "$WORK_DIR" ]
    then
        error "Required parameters"
        cat << EOF
Usage: $0 <OPTIONS>
          --ledgerHost <DAML ledger host>
          --noOfAgreements <No. of Agreements to run DLR simulation>
          --noOfVuser <No. of Users to run DLR simulation>
          --loadBatchSize <Batch size of agreements for an iteration>
          --resultsDir <Results/log directory path>
EOF

        exit 1
    fi
}

load_dlr_application() {
    execute_command --commandToRun "daml json-api --ledger-host ${LEDGER_HOST} --ledger-port ${LEDGER_PORT} --address ${JSON_API_HOST} --http-port ${JSON_API_PORT} --max-inbound-message-size ${JSON_API_MSG_SIZE} --allow-insecure-tokens &"
    sleep 20
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
        error "execute_command(): Missing parameter '--commandToRun <command>'"
        exit 1
    else
        CMD_AS_FILE_NAME=`echo "${COMMAND_TO_RUN}" | tr '/' '_'`
        LOG_FILE="${WORK_SUBDIR}/${CMD_AS_FILE_NAME}".log
        IS_CMD_TO_BE_INITIALIZED_ONCE_FILE="${WORK_DIR}/../${CMD_AS_FILE_NAME}.${LEDGER_HOST}"

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

            info ""

            if [ "${RC}" == "0" ]
            then
                if [ ! -z "${VALIDATE_STRING}" ]
                then
                    keyword_matched=`cat "${LOG_FILE}" | grep "${VALIDATE_STRING}"`
                    if [ ! -z "${keyword_matched}" ]
                    then
                        info "**** Command ${COMMAND_TO_RUN} execution/validation completed successfully"
                    else
                        error "**** Command ${COMMAND_TO_RUN} execution failed"
                        trap_ctrlc
                    fi
                else
                    info "**** Command execution completed successfully"
                fi
            else
                error "**** Command execution failed"
                trap_ctrlc
            fi
        fi
    fi
}

run_dlr_test() {
    info "************************************************************"
    info "Ledger API Host: $LEDGER_HOST"
    execute_command --commandToRun "daml ledger upload-dar --host ${LEDGER_HOST} --port ${LEDGER_PORT} ${DAR_LOCATION}/Rep_App-${REPO_VERSION}.dar" --validateString "DAR upload succeeded." --initializeOnce
    load_dlr_application
    execute_command --commandToRun "node ${REG_LOCATION}/src/Bootstrap.js" --initializeOnce
    sleep 15
    execute_command --commandToRun "k6 run --vus $NO_OF_VUSER --iterations $NO_OF_AGREEMENTS ${REG_LOCATION}/src/Interactions/RegisterAgreements.js" --validateString "✓ is status 200"
    sleep 15
    # Fetch Contract and agreement Ids
    execute_command --commandToRun "node ${REG_LOCATION}/src/Interactions/FetchAgreements.js"
    # Settle all agreements
    execute_command --commandToRun "k6 run --vus 1 --iterations ${ITERATIONS} ${REG_LOCATION}/src/Interactions/SettleAgreementsByKey.js" --validateString "✓ is status 200"
    sleep 15
    # Fetch all agreements, which are in settled status
    execute_command --commandToRun "node ${REG_LOCATION}/src/Interactions/FetchSettledAgreements.js"
    # Calculate interest on all eligible agreements
    execute_command --commandToRun "k6 run --vus 1 --iterations ${ITERATIONS} ${REG_LOCATION}/src/Interactions/CalcInterestByKeys.js" --validateString "✓ is status 200"
    sleep 15
    # Fetch settled status agreements. This is needed as contract ids would have changed
    execute_command --commandToRun "node ${REG_LOCATION}/src/Interactions/FetchSettledAgreements.js"
    # Mature all agreements which are in settled status
    execute_command --commandToRun "k6 run --vus 1 --iterations ${ITERATIONS} ${REG_LOCATION}/src/Interactions/MatureAgreementsByKey.js" --validateString "✓ is status 200"

    info "Successfully completed RunCycleByKeys"
    kill -9 $(ps -ef | grep json-api | awk '{print $2}' | head -3) > /dev/null 2>&1
    EXIT_STATUS=0
}

trap_ctrlc() {
    error "exiting execution"
    kill -9 $(ps -ef | grep json-api | awk '{print $2}' | head -3) > /dev/null 2>&1
    exit 1
}

trap "trap_ctrlc" 2

while [ "$1" != "" ] ; do
    case $1 in
        "--ledgerHost")
            shift
            LEDGER_HOST="$1"
            ;;
        "--noOfAgreements")
            shift
            NO_OF_AGREEMENTS="$1"
            ;;
        "--loadBatchSize")
            shift
            LOAD_BATCH_SIZE="$1"
            ;;
        "--noOfVuser")
            shift
            NO_OF_VUSER="$1"
            ;;
        "--dlrLocation")
            shift
            DLR_LOCATION="$1"
            ;;
        "--resultsDir")
            shift
            WORK_DIR="$1"
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

ITERATIONS=$(( NO_OF_AGREEMENTS / LOAD_BATCH_SIZE ))
DLR_RUN_LOG_FILE="${WORK_SUBDIR}/run.log"
DAR_LOCATION="${DLR_LOCATION}/bk_Broadridge/vmbc-br/rep_app"
REG_LOCATION="${DAR_LOCATION}/orchestration"
export LEDGER_HOST="$LEDGER_HOST"
export LEDGER_PORT="$LEDGER_PORT"
export JSON_API_HOST="$JSON_API_HOST"
export JSON_API_PORT="$JSON_API_PORT"
export JSON_API_MSG_SIZE="$JSON_API_MSG_SIZE"
export REPO_VERSION="$REPO_VERSION"
export LOAD_BATCH_SIZE="$LOAD_BATCH_SIZE"

run_dlr_test 2>&1 > "${DLR_RUN_LOG_FILE}"
info "Exit status for this run: $EXIT_STATUS"
exit "$EXIT_STATUS"

