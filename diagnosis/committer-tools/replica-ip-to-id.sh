#!/bin/bash

source ./my-ipv4.sh

function replica_ip_to_id {
  if [ -z $1 ]; then
    >&2 echo "IP address expected"
    exit 1
  fi

  if [ -z ${CONCORD_CONFIG} ]; then
    CONFIG=/config/concord/config-local/concord.config
  else
    CONFIG=${CONCORD_CONFIG}
  fi

  if [ ! -f ${CONFIG} ]; then
    >&2 echo "Couldn't find ${CONFIG}"
    exit 1
  fi

  IP=$1
  if [ ${IP} = $(my_ipv4) ]; then
    IP="127.0.0.1"
  fi

  yq ".node[] | .replica[] | select(.replica_host==\"${IP}\") | .principal_id" ${CONFIG}
}


[[ ${BASH_SOURCE[0]} = $0 ]] && replica_ip_to_id $@
