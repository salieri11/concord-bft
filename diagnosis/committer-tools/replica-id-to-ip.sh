#!/bin/bash

source ./host-ip.sh

function replica_id_to_ip {
  if [ -z $1 ]; then
    >&2 echo "Replica ID expected"
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

  REPLICA_PATH=$(yq r ${CONFIG} --printMode p "node[*].replica[0].(.==$1)")
  IP=$(yq r ${CONFIG} ${REPLICA_PATH/principal_id/replica_host})

  if [ "${IP}" = "127.0.0.1" ]; then
    host_ip
  else
    echo ${IP}
  fi
}

[[ ${BASH_SOURCE[0]} = $0 ]] && replica_id_to_ip $@
