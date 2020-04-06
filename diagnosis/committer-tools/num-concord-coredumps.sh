#!/bin/bash

CONCORD_COREDUMP_DIR=/config/concord/cores
NUM_CONCORD_COREDUMPS="ls ${CONCORD_COREDUMP_DIR} 2>/dev/null | wc -l || echo 0"
num_concord_coredumps() {
  eval ${NUM_CONCORD_COREDUMPS}
}

[[ ${BASH_SOURCE[0]} = $0 ]] && num_concord_coredumps
