#!/bin/bash

function ssh_probe {
  if [ -z $1 ]; then
    >&2 echo "IP address expected"
    exit 1
  fi

  if [ -z "${SSHPASS}" ]; then
    >&2 echo "Please set SSHPASS"
    exit 1
  fi

  sshpass -e ssh -oStrictHostKeyChecking=no -oConnectTimeout=10 root@$1 exit
}

[[ ${BASH_SOURCE[0]} = $0 ]] && ssh_probe $@
