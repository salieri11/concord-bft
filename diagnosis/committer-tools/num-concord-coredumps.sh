#!/bin/bash

source ./ssh-exec.sh

CONCORD_COREDUMP_DIR=/config/concord/cores
NUM_CONCORD_COREDUMPS="ls ${CONCORD_COREDUMP_DIR} 2>/dev/null | wc -l || echo 0"
function num_concord_coredumps {
  if [ -z "$1" ]; then IP=${HOST_IP}; else IP=$1; fi
  ssh_exec "${IP}" "${NUM_CONCORD_COREDUMPS}"
}

[[ ${BASH_SOURCE[0]} = $0 ]] && num_concord_coredumps $@
