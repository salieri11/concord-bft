#!/bin/sh

while [ "$1" != "" ] ; do
   case $1 in
      "--resultsDir")
         shift
         RESULTS_DIR="$1"
         ;;
   esac
   shift
done

PERFORMANCE_TESTRUN_SUMMARY_FILE="../../../hermes-data/performance_test/perf_testrun_summary.csv"
TRANSACTION_RATE_ALERT_FILE="${RESULTS_DIR}/performance_transaction_rate_spiked.log"
TRANSACTION_RATE_SPIKE_BUFFER=3.00

fetch_perf_transaction_rate() {
    PERFORMANCE_TEST_RESULT_FILE="$1"
    echo "Parsing ${PERFORMANCE_TEST_RESULT_FILE} ..."
    TRANSACTION_RATE=`cat "${PERFORMANCE_TEST_RESULT_FILE}" | grep "Transaction Rate: " | cut -d ':' -f2`
    TRANSACTION_RATE=`echo $TRANSACTION_RATE | awk '{$1=$1}1'`
    echo "TRANSACTION_RATE: $TRANSACTION_RATE"
    if [ ! -z "${TRANSACTION_RATE}" ]
    then
        echo "Updating Performance Transaction Rate..."
        if [ ! -f "${PERFORMANCE_TESTRUN_SUMMARY_FILE}" ]
        then
            echo "Error: File NOT found ${PERFORMANCE_TESTRUN_SUMMARY_FILE}"
            exit 1
        fi
        echo "\t**** Transaction Rate: $TRANSACTION_RATE"
        echo "`date +%D`,$TRANSACTION_RATE" >> "${PERFORMANCE_TESTRUN_SUMMARY_FILE}"

        if [ ! -z "${WORKSPACE}" ]
        then
            echo "Copying Performance Testrun Summary file to ${WORKSPACE} for graph"
            cp "${PERFORMANCE_TESTRUN_SUMMARY_FILE}" "${WORKSPACE}"
        fi

        # Check if this run has spiked up
        check_for_spiked_run "$TRANSACTION_RATE"
    else
        echo "Error finding \"Transaction Rate\". Aborting"
        exit 1
    fi
}

check_for_spiked_run() {
    echo "Checking if this run has dipped..."

    current_transaction_rate=$1
    no_of_lines=`cat "${PERFORMANCE_TESTRUN_SUMMARY_FILE}" | wc -l`
    if [ "$no_of_lines" -gt "2" ]
    then
        previous_transaction_rate=`tail -2 "${PERFORMANCE_TESTRUN_SUMMARY_FILE}" | head -1 | cut -d, -f2`
    fi

    if [ ! -z "${previous_transaction_rate}" ]
    then
        echo "\tTransaction Rate from Previous Run: $previous_transaction_rate"
        echo "\tTransaction Rate from Current Run: $current_transaction_rate"

        transaction_rate_diff=`echo $previous_transaction_rate - $current_transaction_rate | bc`
        if [ 1 -eq "$(echo "${transaction_rate_diff} > ${TRANSACTION_RATE_SPIKE_BUFFER}" | bc)" ]
        then
            echo "\n\t**** Transaction Rate has dipped in this run ***"
            echo "Creating log file for Alert Notification: ${TRANSACTION_RATE_ALERT_FILE}"
            echo "Transaction Rate from Previous Run: $previous_transaction_rate" > "${TRANSACTION_RATE_ALERT_FILE}"
            echo "Transaction Rate from Current Run: $current_transaction_rate" >> "${TRANSACTION_RATE_ALERT_FILE}"
        else
            echo "\tDifference falls within permitted buffer ($TRANSACTION_RATE_SPIKE_BUFFER)"
        fi
    else
        echo "WARNING: No Transaction Rate found from previous runs"
    fi
}


if [ -z "${RESULTS_DIR}" ]
then
    echo "Usage: $0 --resultsDir <path to results dir>"
    exit 1
elif [ ! -d "${RESULTS_DIR}" ]
then
    echo "Results Dir: ${RESULTS_DIR} NOT found"
    exit 1
else
    cd "${RESULTS_DIR}"/PerformanceTests_*
    PERFORMANCE_TESTRUN_RESULT_DIR=`pwd`
    PERFORMANCE_TEST_RESULT_FILE="$PERFORMANCE_TESTRUN_RESULT_DIR/test_logs/performance_test/performance_result.log"
fi

if ls "${PERFORMANCE_TEST_RESULT_FILE}" 1> /dev/null 2>&1 ; 
then
    fetch_perf_transaction_rate "${PERFORMANCE_TEST_RESULT_FILE}"
    # Copy for providing link in alert notification email 
    cp "${PERFORMANCE_TEST_RESULT_FILE}" "${RESULTS_DIR}"
    exit 0
else
    echo "Performance result file NOT found. Aborting"
    exit 1
fi

