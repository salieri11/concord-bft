#!/bin/bash

source ./ssh-exec.sh

CONCORD_CONTAINER_STATUS="docker container inspect concord | jq '.[0].State.Status' | tr -d '\"'"
function concord_container_status {
  if [ -z "$1" ]; then IP=${HOST_IP}; else IP=$1; fi
  ssh_exec "${IP}" "${CONCORD_CONTAINER_STATUS}"
}

[[ ${BASH_SOURCE[0]} = $0 ]] && concord_container_status $@
