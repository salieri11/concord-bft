#!/bin/sh

while [ "$1" != "" ] ; do
   case $1 in
      "--testsuite")
         shift
         TEST_SUITES=$1
         ;;
      "--noOfRuns")
         shift
         NO_OF_RUNS=$1
         ;;
      "--tests")
         shift
         SPECIFIC_TESTS=$1
         ;;
   esac
   shift
done

TIME_STAMP=`date +%m%d%Y_%H%M%S`
BASE_LOG_DIR=/var/log/vmwblockchain
RESULTS_DIR=${BASE_LOG_DIR}/memory_leak_testrun_${TIME_STAMP}
MEMORY_INFO_LOG_FILE=${RESULTS_DIR}/memory_info_${TIME_STAMP}.log
MEMORY_INFO_CSV_FILE=${RESULTS_DIR}/memory_info_${TIME_STAMP}.csv
SLEEP_TIME_IN_SEC=60
MEMORY_LEAK_PASS_FILE="${RESULTS_DIR}/test_status.pass"
HERMES_START_FILE="./main.py"
RC=1

check_usage() {
    if [ "x${TEST_SUITES}" = "x" -o "x${NO_OF_RUNS}" = "x" ]
    then
        echo "Usage: $0 --testsuite <Testsuite to run e.g CoreVMTests> --noOfRuns <No. of times to repeat complete test runs>"
        exit 1
    fi
}

launch_memory_test() {
    CWD=$(pwd)
    cd ..
    "${HERMES_START_FILE}" "${TEST_SUITES}" --config resources/user_config_valgrind.json --noOfRuns ${NO_OF_RUNS} --resultsDir ${RESULTS_DIR} --tests ${SPECIFIC_TESTS} &
    PID=$!

    cd $CWD
    while true
    do
        is_process_still_running=`ps -ef | grep "${HERMES_START_FILE}" | grep $PID`
        if [ "x$is_process_still_running" = "x" ]
        then
            sleep 5
            echo "Done running memory leak tests"
            if [ -f "${MEMORY_LEAK_PASS_FILE}" ]
            then
                echo "Memory Leak Test Passed"
                RC=0
            else
                echo "Memory Leak Test Failed"
                RC=1
            fi
            echo "Results: ${RESULTS_DIR}"
            exit $RC
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

