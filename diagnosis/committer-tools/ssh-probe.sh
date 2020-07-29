#!/bin/bash

source ./ssh-exec.sh

function ssh_probe {
  ssh_exec $1 exit
}

[[ ${BASH_SOURCE[0]} = $0 ]] && ssh_probe $@
