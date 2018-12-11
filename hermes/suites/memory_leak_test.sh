#!/bin/sh

while [ "$1" != "" ] ; do
   case $1 in
      "--testSuite")
         shift
         TEST_SUITE=$1
         ;;
      "--repeatSuiteRun")
         shift
         NO_OF_RUNS=$1
         ;;
      "--tests")
         shift
         TESTS=$1
         ;;
   esac
   shift
done

retVal=1
TIME_STAMP=`date +%m%d%Y_%H%M%S`
BASE_LOG_DIR=/var/log/vmwblockchain
RESULTS_DIR=${BASE_LOG_DIR}/memory_leak_testrun_${TIME_STAMP}
MEMORY_INFO_LOG_FILE=${RESULTS_DIR}/memory_info_${TIME_STAMP}.log
MEMORY_INFO_CSV_FILE=${RESULTS_DIR}/memory_info_${TIME_STAMP}.csv
SLEEP_TIME_IN_SEC=60
MEMORY_LEAK_PASS_FILE="${RESULTS_DIR}/test_status.pass"
concord1_VALGRIND_LOG_FILE="/tmp/valgrind_concord1.log"
HERMES_START_FILE="./main.py"
SPECIFIC_TESTS=""

check_usage() {
    if [ "x${TEST_SUITE}" = "x" -o "x${NO_OF_RUNS}" = "x" ]
    then
        echo "Usage: $0 --testSuite <Testsuite to run e.g CoreVMTests> --repeatSuiteRun <No. of times to repeat complete test runs>"
        exit 1
    fi
}

launch_memory_test() {
    CWD=$(pwd)
    cd ..
    if [ ! "x${TESTS}" = "x" ]
    then
        SPECIFIC_TESTS="--tests ${TESTS}"
    fi
    "${HERMES_START_FILE}" "${TEST_SUITE}" --config resources/user_config_valgrind.json --repeatSuiteRun ${NO_OF_RUNS} --resultsDir ${RESULTS_DIR} ${SPECIFIC_TESTS} --dockerComposeFile "" &
    PID=$!

    cd $CWD
    while true
    do
        is_process_still_running=`ps -ef | grep "${HERMES_START_FILE}" | grep $PID`
        if [ "x$is_process_still_running" = "x" ]
        then
            sleep 5
            echo "Done running memory leak tests"
            if [ -f "${concord1_VALGRIND_LOG_FILE}" ]
            then
                mv "${concord1_VALGRIND_LOG_FILE}" "${RESULTS_DIR}"
            fi
            echo "Results: ${RESULTS_DIR}"
            break
        fi
        memory_info=`free -m`
        total_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f2`
        used_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f3`
        free_memory=`echo "${memory_info}" | grep "Mem:" | tr -s " " | cut -d" " -f7`
        echo "${memory_info}"
        echo "`date +%m/%d/%Y\ %T`,${total_memory},${used_memory},${free_memory}" >> ${MEMORY_INFO_CSV_FILE}
        sleep ${SLEEP_TIME_IN_SEC}
    done
}

if [ ! -d "${RESULTS_DIR}" ]
then
    mkdir -p ${RESULTS_DIR}
fi

check_usage
launch_memory_test 2>&1 | tee ${MEMORY_INFO_LOG_FILE}
if [ -f "${MEMORY_LEAK_PASS_FILE}" ]
then
    echo "Memory Leak Test Passed"
    retVal=0
else
    echo "Memory Leak Test Failed"
    retVal=1
fi

echo "Exit status: $retVal"
exit $retVal
