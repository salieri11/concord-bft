#!/bin/bash

MY_IPv4="ip addr show eth0 | grep 'inet ' | awk '{split(\$2,a,\"/\"); print a[1]}'"
my_ipv4() {
  eval ${MY_IPv4}
}

[[ ${BASH_SOURCE[0]} = $0 ]] && my_ipv4
