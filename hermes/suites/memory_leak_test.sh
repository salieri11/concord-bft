#!/bin/sh

while [ "$1" != "" ] ; do
   case $1 in
      "--testSuite")
         shift
         TEST_SUITE="$1"
         ;;
      "--repeatSuiteRun")
         shift
         NO_OF_RUNS=$1
         ;;
      "--tests")
         shift
         TESTS="$1"
         ;;
      "--resultsDir")
         shift
         RESULTS_DIR="$1"
         ;;
      "--suitesRealname")
         shift
         SUITES_REALNAME="$1"
         ;;
   esac
   shift
done

echo Parsed arguments:
echo - TEST_SUITE: "${TEST_SUITE}"
echo - NO_OF_RUNS: "${NO_OF_RUNS}"
echo - TESTS: "${TESTS}"
echo - RESULTS_DIR: "${RESULTS_DIR}"
echo - SUITES_REALNAME: "${SUITES_REALNAME}"

retVal=1
TIME_STAMP=`date +%m%d%Y_%H%M%S`
BASE_LOG_DIR=/var/log/vmwblockchain
if [ -z "${RESULTS_DIR}" ]
then
    RESULTS_DIR="${BASE_LOG_DIR}/memory_leak_testrun_${TIME_STAMP}"
fi
MEMORY_INFO_LOG_FILE="${RESULTS_DIR}"/memory_info_${TIME_STAMP}.log
MEMORY_INFO_CSV_FILE="${RESULTS_DIR}"/memory_info_${TIME_STAMP}.csv
SLEEP_TIME_IN_SEC=60
MEMORY_LEAK_PASS_FILE="${RESULTS_DIR}/test_status.pass"
VALGRIND_LOG_FILENAME="valgrind_concord1.log"
concord1_VALGRIND_LOG_FILE="/tmp/$VALGRIND_LOG_FILENAME"
HERMES_START_FILE="./main.py"
SPECIFIC_TESTS=""
HERMES_PID_FILE="/tmp/hermes_pid"
# to represent leak summary on graph
# memory leak summary data gets saved in repo: hermes-data
MEMORY_LEAK_SUMMARY_FILE="../../../hermes-data/memory_leak_test/memory_leak_summary.csv"
MEMORY_LEAK_ALERT_FILE="${RESULTS_DIR}/memory_leak_spiked.log"
NONLEAK_ALERT_FILE="${RESULTS_DIR}/nonleak_alerts.log"
LEAK_SPIKE_BUFFER=500

check_usage() {
    if [ "x${TEST_SUITE}" = "x" -o "x${NO_OF_RUNS}" = "x" ]
    then
        echo "Usage: $0 --testSuite <Testsuite to run e.g EthCoreVmTests> --repeatSuiteRun <No. of times to repeat complete test runs>"
        exit 1
    fi
}

launch_memory_test() {
    CWD="$(pwd)"
    cd ..
    if [ ! "x${TESTS}" = "x" ]
    then
        SPECIFIC_TESTS="\"--tests=-k ${TESTS}\""
    fi

    COMMAND="\"${HERMES_START_FILE}\" \"${TEST_SUITE}\" --config resources/user_config_valgrind.json --repeatSuiteRun ${NO_OF_RUNS} --suitesRealname ${SUITES_REALNAME} --resultsDir \"${RESULTS_DIR}\" ${SPECIFIC_TESTS} --productLaunchAttempts 10  --runConcordConfigurationGeneration --concordConfigurationInput /concord/config/dockerConfigurationInput.yaml --dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-memleak.yml --logLevel debug > \"${RESULTS_DIR}/memory_leak_tests.log\" 2>&1 &"
    echo python: "${python}"
    echo COMMAND: "${COMMAND}"
    eval "${python}" "${COMMAND}"

    HERMES_PID=$!
    rm -f "${HERMES_PID_FILE}"
    echo ${HERMES_PID} > "${HERMES_PID_FILE}"
    echo "Hermes process ID ${HERMES_PID} written to ${HERMES_PID_FILE}"

    cd "$CWD"
    while true
    do
        is_process_still_running=`ps -ef | grep "${HERMES_START_FILE}" | grep ${HERMES_PID}`
        if [ "x$is_process_still_running" = "x" ]
        then
            sleep 5
            echo "Done running Memory Leak Tests"
            if [ -f "${concord1_VALGRIND_LOG_FILE}" ]
            then
                echo "Valgrind Log file: ${concord1_VALGRIND_LOG_FILE} Found"
                mv "${concord1_VALGRIND_LOG_FILE}" "${RESULTS_DIR}"
            else
                echo "Valgrind Log file: ${concord1_VALGRIND_LOG_FILE} NOT Found"
                exit 1
            fi
            echo "Results: ${RESULTS_DIR}"
            break
        fi
        memory_info=`free -m`
        total_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f2`
        used_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f3`
        free_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f7`
        echo "Updating ${MEMORY_INFO_CSV_FILE} with Memory Info: ${memory_info}"
        echo "`date +%m/%d/%Y\ %T`,${total_memory},${used_memory},${free_memory}" >> "${MEMORY_INFO_CSV_FILE}"
        sleep ${SLEEP_TIME_IN_SEC}
    done
}

trap_ctrlc() {
    if [ -f "${HERMES_PID_FILE}" ]
    then
        read HERMES_PID < "${HERMES_PID_FILE}"
        if [ "$HERMES_PID" != "" ]
        then
            echo "Interrupt detected after Hermes launch. Killing Hermes process ${HERMES_PID}"
            kill "${HERMES_PID}"
            rm -f "${HERMES_PID_FILE}"
            echo "Killing and removing all docker containers"
            docker kill $(docker ps -aq)
            docker rm $(docker ps -aq)
        fi
    fi
}

fetch_leak_summary() {
    leak_summary=`awk '/LEAK SUMMARY/{getline; print}' "${RESULTS_DIR}/${VALGRIND_LOG_FILENAME}" | grep -oP "definitely lost: .{0,10}" | cut -d ":" -f2 | cut -d " " -f 2 | tr -d ','`
    if [ "$leak_summary" != "" ]
    then
        echo "Updating memory leak summary..."
        if [ ! -f ${MEMORY_LEAK_SUMMARY_FILE} ]
        then
            echo "\"Date\"","\"Memory Leak Summary\"" > ${MEMORY_LEAK_SUMMARY_FILE}
        fi
        echo "\t**** LEAK SUMMARY: $leak_summary bytes"
        echo "`date +%D`,$leak_summary" >> ${MEMORY_LEAK_SUMMARY_FILE}

        if [ "${WORKSPACE}" != "" ]
        then
            echo "Copying Memory leak summary file to ${WORKSPACE} for graph"
            cp "${MEMORY_LEAK_SUMMARY_FILE}" "${WORKSPACE}"
        fi

        # Check if Memory LEAK has spiked up
        check_for_spiked_mem_leak "$leak_summary"
    else
        echo "$MEMORY_LEAK_SUMMARY_FILE NOT updated with LEAK SUMMARY. Aborting."
        exit 1
    fi
}

check_for_spiked_mem_leak() {
    echo "Checking if Memory LEAK has Spiked up..."

    current_leak=$1
    no_of_lines=`cat ${MEMORY_LEAK_SUMMARY_FILE} | wc -l`
    if [ "$no_of_lines" -gt "2" ]
    then
        previous_leak=`tail -2 ${MEMORY_LEAK_SUMMARY_FILE} | head -1 | cut -d, -f2`
    fi

    if [ ! "${previous_leak}" = "" ]
    then
        echo "\tMemory LEAK from Previous Run: $previous_leak"
        echo "\tMemory LEAK from Current Run: $current_leak"

        leak_diff=`expr $current_leak - $previous_leak`
        if [ "$leak_diff" -gt "$LEAK_SPIKE_BUFFER" ]
        then
            echo "\n\t**** Memory LEAK has spiked up in this run ***"
            echo "Memory LEAK from Previous Run: $previous_leak" > "${MEMORY_LEAK_ALERT_FILE}"
            echo "Memory LEAK from Current Run: $current_leak" >> "${MEMORY_LEAK_ALERT_FILE}"

            echo "Creating log file for Alert Notification: ${MEMORY_LEAK_ALERT_FILE}"
        else
            echo "\tDifference falls within permitted buffer ($LEAK_SPIKE_BUFFER bytes)"
        fi
    else
        echo "WARNING: No Memory LEAK data found from previous runs"
    fi
}

# Alert about non-leak warnings
check_nonleak_warnings() {
    uninitialised_variables=`grep -A3 -F "Conditional jump or move depends on uninitialised value" "${RESULTS_DIR}/${VALGRIND_LOG_FILENAME}"`

    if [ ! -z "${uninitialised_variables}" ]
    then
        echo "\n\t**** Uninitialised variables found in this run ***"
        echo "Uninitialised variable warnings:\n$uninitialised_variables" > "${NONLEAK_ALERT_FILE}"
        echo "Creating log file for Alert Notification: ${NONLEAK_ALERT_FILE}"
    fi
}

trap "trap_ctrlc" 2
check_usage

if [ ! -d "${RESULTS_DIR}" ]
then
    mkdir -p "${RESULTS_DIR}"
fi

if [ -f "${MEMORY_LEAK_ALERT_FILE}" ]
then
    rm -f "${MEMORY_LEAK_ALERT_FILE}"
fi

if [ -f "${NONLEAK_ALERT_FILE}" ]
then
    rm -f "${NONLEAK_ALERT_FILE}"
fi

# skip if pipeline is only doing a dry run without testing
if test -f "${WORKSPACE}/summary/dry_run.json"; then
    exit 0
fi

launch_memory_test 2>&1 | tee "${MEMORY_INFO_LOG_FILE}"

check_nonleak_warnings

if ( [ -f "${MEMORY_LEAK_PASS_FILE}" ] && [ ! -f "${NONLEAK_ALERT_FILE}" ] )
then
    echo "Memory Leak Test Passed"
    fetch_leak_summary
    retVal=0
else
    echo "Memory Leak Test Failed"
    retVal=1
fi

rm -f "${HERMES_PID_FILE}"

echo "Exit status: $retVal"
exit $retVal
