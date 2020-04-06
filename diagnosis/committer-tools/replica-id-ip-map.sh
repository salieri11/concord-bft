#!/bin/bash

source ./my-ipv4.sh

if [ -z ${CONCORD_CONFIG} ]; then
  CONFIG=/config/concord/config-local/concord.config
else
  CONFIG=${CONCORD_CONFIG}
fi

if [ ! -f ${CONFIG} ]; then
  >&2 echo "Couldn't find ${CONFIG}"
  exit 1
fi

TMP=/tmp/diagnosis
mkdir -p ${TMP}

yq r ${CONFIG} 'node[*].replica[*].principal_id' > ${TMP}/ids
yq r ${CONFIG} 'node[*].replica[*].replica_host' > ${TMP}/ips
sed -i "s/127.0.0.1/$(my_ipv4)/g" ${TMP}/ips
paste ${TMP}/ids ${TMP}/ips | sort -n

rm -rf ${TMP}
