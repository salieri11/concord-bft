#!/bin/bash

# Query the host with the following command
# $ ip addr show <interface> | sed -nE 's#^.*inet (.*)/.*$#\1#p'

function host_ip {
  if [ -z ${HOST_IP} ]; then
    >&2 echo "HOST_IP environment variable not set"
    exit 1
  fi
  echo ${HOST_IP}
}

[[ ${BASH_SOURCE[0]} = $0 ]] && host_ip
