#!/bin/bash

#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file contains code related to launching and monitoring build
# processes.  e.g. buildall.sh handles command line parameters and creates
# the commands used to build things.  This file receives process IDs,
# monitors processes, re-launches them if needed, reports on success/
# failure, etc...
#########################################################################

trap killAllProcs INT

# Kill all processes.
killAllProcs(){
  error "A problem or interrupt occurred. Killing processes..."
  saveTimeEvent "Kill all processes" Start

  for i in "${!BUILD_PROCESS_IDS[@]}"; do
    if isRunning ${BUILD_PROCESS_IDS[$i]}
    then
      kill ${BUILD_PROCESS_IDS[$i]}
    fi
  done

  saveTimeEvent "Kill all processes" End
}


info() {
    echo `date`: INFO: "${1}"
}


error() {
    echo `date`: ERROR: "${1}" 1>&2
}


printMemory() {
    uname | grep "Linux"

    if [ $? -eq 0 ]; then
        free -h
    fi
}


# Accept a process ID and return 0/success if running,
# nonzero/failure if not running.
isRunning(){
  ps -p ${1} > /dev/null
  return $?
}


# Checks to see if the target passed in has already succeeded.
# Used instead of asking the OS for the exit status of a process
# once we already know the answer.
alreadySucceeded() {
  TARGET="${1}"

  for SUCCESS_ITEM in "${SUCCESSES[@]}"; do
    if [ "${SUCCESS_ITEM}" == "${TARGET}" ]; then
      return 0
    fi
  done

  return 1
}


# Waits for all processes in the BUILD_PROCESS_IDS associative array to end.
waitForProcesses(){
  local DONE=false

  while [ "$DONE" = false ]; do
    DONE=true

    info "-------- Status --------"
    printMemory

    for BUILD_NAME in "${!BUILD_PROCESS_IDS[@]}"; do
      if alreadySucceeded "${BUILD_NAME}"; then
        info "${BUILD_NAME}: done"
      elif isRunning "${BUILD_PROCESS_IDS[$BUILD_NAME]}"; then
        DONE=false
	info "${BUILD_NAME}: waiting"
      else
        if ! retryIfNeeded "${BUILD_NAME}"; then
          DONE=false
        else
          saveTimeEvent "Build ${BUILD_NAME}" End
        fi
      fi
    done

    sleep 5
  done
}


retryIfNeeded(){
  # We just finished with this build.  See what we need to do.
  local BUILD_NAME="${1}"

  if buildSucceeded "${BUILD_NAME}"; then
    info "${BUILD_NAME}: done"
    SUCCESSES+=("${BUILD_NAME}")
    return 0
  fi

  local FAIL_RUN=false

  if isInfraBuildError "${BUILD_NAME}"; then
      info "Found a reason to retry ${BUILD_NAME}"
      BUILD_FAILURES["${BUILD_NAME}"]=$((BUILD_FAILURES["${BUILD_NAME}"]+1))

      if [ ${BUILD_FAILURES["${BUILD_NAME}"]} -ge ${MAX_FAILURES} ]; then
          error "Build of ${BUILD_NAME} has failed and reached the maximum number of attempts.  Failing."
          FAIL_RUN=true
      else
          retryBuild "${BUILD_NAME}"
          return 1
      fi
  else
    error "Build of ${BUILD_NAME} failed, and no reason to retry it was detected."
    FAIL_RUN=true
  fi

  if [ $FAIL_RUN = true ]; then
    printBuildFailure "${BUILD_NAME}"
    killAllProcs
    exit 1
  fi
}


printBuildFailure(){
  # Ignore leading blank spaces, do a case insensitive search for "error" in the first
  # few characters, and then show the next several lines.
  local BUILD_NAME="${1}"
  local LOG_FILE=`pwd`/${BUILD_LOGS["${BUILD_NAME}"]}
  echo "${BUILD_NAME}" >> "${WORKSPACE}/summary/failed_build_name.log" # save failed build name.
  echo "${LOG_FILE}" >> "${WORKSPACE}/summary/failed_build_path.log" # save failed build log file path.
  info "================================================================================"
  info "=========================== Attempting auto-triage! ============================"
  info "================================================================================"
  info "Searching ${LOG_FILE} for errors.  Results:"
  grep -A10 -i "^ *.\{0,5\}error" "${LOG_FILE}" | tee "${WORKSPACE}/summary/failed_build_error.log"
  if [ $? -ne 0 ]; then
      info "No lines with 'error' near the beginning found. Here are the last few lines of the log:"
      tail -n 10 "${LOG_FILE}" | tee "${WORKSPACE}/summary/failed_build_error.log"
  fi
  info "================================================================================"
  info "================================================================================"
  info "================================================================================"
}


retryBuild(){
    # Called when a build has failed and we have decided to retry it.
    # Given the build name, updates the log name and re-launches the command.
    local BUILD_NAME="${1}"
    local CMD=${BUILD_COMMANDS["${BUILD_NAME}"]}
    local OLD_LOG=${BUILD_LOGS["${BUILD_NAME}"]}
    local BASE=""

    info "Retrying build of ${BUILD_NAME}"

    if echo "${OLD_LOG}" | grep "_retry[0-9]*\.log" > /dev/null; then
      BASE=`echo "${OLD_LOG}" | sed 's/_retry[0-9]*\.log//g'`
    else
      BASE=`echo "${OLD_LOG}" | sed 's/\.log//g'`
    fi

    local NEW_LOG="${BASE}_retry${BUILD_FAILURES[${BUILD_NAME}]}.log"
    eval "(${CMD} > ${NEW_LOG} 2>&1) &"
    addToProcList "${BUILD_NAME}" $! "${NEW_LOG}"
}


isInfraBuildError(){
    # Call functions to look for indications that we should retry
    # a build due to an infra error, like a 503 from Artifactory
    # during a Maven build.
    # Return 0/success if we should, 1/failure if not.
    local BUILD_NAME="${1}"

    if grepLogForErrors "${BUILD_NAME}" || \
            findEOFArtifactoryError "${BUILD_NAME}"; then
        return 0
    else
        return 1
    fi
}


grepLogForErrors(){
    local BUILD_NAME="${1}"
    local LOG_FILE=${BUILD_LOGS["${BUILD_NAME}"]}
    local ERROR_STRINGS=("\[ERROR\].*Could not transfer.* 50[23]" \
                         "\[ERROR\].*Could not transfer.* Connection reset" \
                         "sbt.librarymanagement.ResolveException: download failed:" \
                         "Too Many Requests (HAP429)" \
                         "docker\.io.*Client\.Timeout exceeded" \
                         "curl: (56) SSL read: error:00000000:lib(0):func(0):reason(0), errno 104" \
                         "Failed to fetch.*Undetermined Error" \
                         "download error: Caught java.io.IOException" \
                         "TLS handshake timeout")

    info "Looking for errors in build log file ${LOG_FILE} to see if we should retry."

    if [ -f "${LOG_FILE}" ]; then
      for ERR in "${ERROR_STRINGS[@]}"; do
        grep -e "${ERR}" "${LOG_FILE}"

        if [ $? -eq 0 ]; then
            return 0
        fi
      done
    else
      error "Did not find the log file."
      return 1
    fi

    return 1
}


findEOFArtifactoryError(){
    # Look for "unexpected EOF" as the last line of a log, which
    # happens when failing to pull a docker image from Artifactory.
    local BUILD_NAME="${1}"
    local LOG_FILE=${BUILD_LOGS["${BUILD_NAME}"]}
    info "Looking for unexpected EOF error in the last line of build log file ${LOG_FILE} to see if we should retry."

    if [ -f "${LOG_FILE}" ]; then
      local LAST_LINE=`tail -1l "${LOG_FILE}"`
      echo "${LAST_LINE}" | grep -e "^unexpected\ EOF"
      return $?
    else
      error "Did not find the log file."
      return 1
    fi
}


buildSucceeded(){
  # Accepts a build name and returns 0/success or 1/failure.
  local BUILD_NAME="${1}"

  getBuildExitCode "${BUILD_NAME}" && return 0 || return 1
}


getBuildExitCode(){
  # Accepts a build name, fetches its process ID, and
  # returns the exit code of that process.
  local BUILD_NAME="${1}"
  local PID=${BUILD_PROCESS_IDS["${BUILD_NAME}"]}
  wait ${PID}
  return $?
}


# Accepting a human-friendly name and process ID, adds
# them to the BUILD_PROCESS_IDS array of name=procId.
addToProcList(){
  local BUILD_NAME="${1}"
  local BUILD_PROCESS_ID="${2}"
  local LOG_FILE="${3}"
  local BUILD_CMD=`ps -p ${BUILD_PROCESS_ID} -o args --no-headers`
  info "Adding build process: ${BUILD_NAME}"

  saveTimeEvent "Build ${BUILD_NAME}" Start

  BUILD_PROCESS_IDS["${BUILD_NAME}"]=${BUILD_PROCESS_ID}
  BUILD_COMMANDS["${BUILD_NAME}"]="${BUILD_CMD}"
  BUILD_LOGS["${BUILD_NAME}"]="${LOG_FILE}"

  if [ -z ${BUILD_FAILURES["${BUILD_NAME}"]} ]; then
    BUILD_FAILURES["${BUILD_NAME}"]=0
  fi
}


# For each of these, the key is the name, like "concord-core_image".
# Shell only supports strings as values, so we can't create arrays of arrays.
# We should think about a real build tool.
declare -A BUILD_PROCESS_IDS # Build Name => Process ID
declare -A BUILD_COMMANDS    # Build Name => Process command
declare -A BUILD_FAILURES    # Build Name => Number of build failures
declare -A BUILD_LOGS        # Build Name => Current log file for this build (changes with retries)
MAX_FAILURES=3               # We will retry a build failure due to environmental issues like Artifactory flakiness.

# Build names are added to this array when they are successful.
SUCCESSES=()
