#!/bin/bash

MY_IPv4="ip addr show eth0 | grep 'inet ' | awk '{split(\$2,a,\"/\"); print a[1]}'"
my_ipv4() {
  eval ${MY_IPv4}
}

if [ "${#BASH_SOURCE[@]}" -eq 1 ]; then
  my_ipv4
fi
