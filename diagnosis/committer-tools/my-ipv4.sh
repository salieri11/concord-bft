#!/bin/bash

# Query the host with the following command
# $ ip addr show <interface> | sed -nE 's#^.*inet (.*)/.*$#\1#p'

my_ipv4() {
  if [ -z ${MY_IP} ]; then
    >&2 echo "MY_IP environment variable not set"
    exit 1
  fi
  echo ${MY_IP}
}

[[ ${BASH_SOURCE[0]} = $0 ]] && my_ipv4
