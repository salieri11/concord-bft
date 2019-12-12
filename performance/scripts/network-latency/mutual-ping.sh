#!/bin/bash

function doSsh() {
  local host=$1
  local command=$2
  sshpass -e ssh -q -o "StrictHostKeyChecking no" root@"$host" "$command"
}

function findHosts() {
  local host=$1
  local file="/config/concord/config-local/concord.config"
  local command="cat $file | grep replica_host | grep -Eo '([0-9]{1,3}[\.]){3}[0-9]{1,3}'"
  hosts=$(doSsh "$host" "$command")
  hosts=("${hosts[@]/127.0.0.1/$host}")
}

function openFirewall() {
  local host=$1
  local command="iptables -A INPUT -p icmp --icmp-type echo-request -j ACCEPT"
  doSsh "$host" "$command"
}

function closeFirewall() {
  local host=$1
  local command="iptables -D INPUT -p icmp --icmp-type echo-request -j ACCEPT"
  doSsh "$host" "$command"
}

# shellcheck disable=SC2207
function doPing() {
  local host=$1
  local target=$2
  local command="ping -i .001 -q -c 1000 $target | grep rtt | grep -Eo [0-9]+\.[0-9]{3} | tr '\n' ' '"
  rtts=($(doSsh "$host" "$command"))
}

function doPings() {
  local host=$1
  local targets=$2
  echo
  echo "Ping RTT statistics (ms). Source: $host"
  echo "|--------------|-------|-------|-------|-------|"
  echo "|    Target    |  Min  |  Avg  |  Max  | MDev  |"
  echo "|--------------|-------|-------|-------|-------|"
  for target in $targets; do
    if [ "$target" != "$host" ]; then
      doPing "$host" "$target"
      printf "| %s |" "$target"
      printf " %s |" "${rtts[@]}"
      echo
    fi
  done
  echo "|--------------|-------|-------|-------|-------|"
}

# shellcheck disable=SC2128
function checkLatency() {
  local host=$1
  findHosts "$host"

  for host in $hosts; do
    openFirewall "$host"
  done

  for host in $hosts; do
    doPings "$host" "$hosts"
  done

  for host in $hosts; do
    closeFirewall "$host"
  done
}

# Validate
if [ $# -eq 0 ]; then
  echo "usage: $0 <IP>"
  exit 1
fi

if [ -z "${SSHPASS}" ]; then
  echo "Please set root password (export SSHPASS=xxxx)."
  exit 1
fi

# Main
checkLatency "$1"