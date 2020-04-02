#!/bin/bash

CONCORD_CONTAINER_STATUS="docker container inspect concord | jq '.[0].State.Status' | tr -d '\"'"
concord_container_status() {
  eval ${CONCORD_CONTAINER_STATUS} 2>/dev/null
}

[[ ${BASH_SOURCE[0]} = $0 ]] && concord_container_status
