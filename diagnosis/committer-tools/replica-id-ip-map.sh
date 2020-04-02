#!/bin/bash

if [ -z ${CONCORD_CONFIG} ]; then
  CONFIG=/config/concord/config-local/concord.config
else
  CONFIG=${CONCORD_CONFIG}
fi

if [ ! -f ${CONFIG} ]; then
  >&2 echo "Couldn't find ${CONFIG}"
  exit 1
fi

MAPPING=$(cat ${CONFIG} | yq '.node[] | .replica[] | "\(.principal_id):\(.replica_host):\(.replica_port)"')
for kv in ${MAPPING}; do
    kv=${kv%\"}
    kv=${kv#\"}
    kv=${kv/:/$'\t'}
    echo ${kv}
done
