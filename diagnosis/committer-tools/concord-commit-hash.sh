#!/bin/bash

if [ -z $1 ]; then
  CONCORD_IMAGE=$(docker images | grep "concord-core" | awk '{print $1 ":" $2}')
else
  CONCORD_IMAGE=$(docker image ls $1 | sed '1d' | awk '{print $1 ":" $2}')
fi

if [ -z "${CONCORD_IMAGE}" ]; then
    >&2 echo "Coulnd't find concord docker image"
    exit 1
fi

for concord in ${CONCORD_IMAGE}; do
  hash=$(docker image inspect ${concord} | jq '.[0].ContainerConfig.Labels["com.vmware.blockchain.commit"]')
  if [ "${hash}" != "null" ]; then
    hash=${hash%\"}
    hash=${hash#\"}
    echo ${hash}
  fi
done
