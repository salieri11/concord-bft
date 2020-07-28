#!/bin/bash

source ./my-ipv4.sh

function replica_ips {
  if [ -z ${CONCORD_CONFIG} ]; then
    CONFIG=/config/concord/config-local/concord.config
  else
    CONFIG=${CONCORD_CONFIG}
  fi

  if [ ! -f ${CONFIG} ]; then
    >&2 echo "Couldn't find ${CONFIG}"
    exit 1
  fi

  IPS=$(yq r ${CONFIG} 'node[*].replica[0].replica_host')
  for ip in ${IPS}; do
    if [ "${ip}" = "127.0.0.1" ]; then
      my_ipv4
    else
      echo ${ip}
    fi
  done
}

[[ ${BASH_SOURCE[0]} = $0 ]] && replica_ips
