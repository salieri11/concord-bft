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

  IPS=$(cat ${CONFIG} | yq ".node[] | .replica[] | .replica_host")
  for ip in ${IPS}; do
    ip=${ip%\"}
    ip=${ip#\"}
    if [ "${ip}" = "127.0.0.1" ]; then
      ip=$(my_ipv4)
    fi
    echo ${ip}
  done
}

[[ ${BASH_SOURCE[0]} = $0 ]] && replica_ips
