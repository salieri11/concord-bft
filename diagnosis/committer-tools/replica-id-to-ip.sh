#!/bin/bash

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

  IP=$(yq ".node[] | .replica[] | select(.principal_id==$1) | .replica_host" ${CONFIG})
  IP=${IP%\"}
  IP=${IP#\"}
  echo ${IP}
}

[[ ${BASH_SOURCE[0]} = $0 ]] && replica_id_to_ip $@
