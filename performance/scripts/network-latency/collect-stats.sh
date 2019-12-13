#!/bin/bash

do_ssh() {
  local host=$1
  local command=$2
  sshpass -e ssh -q -o "StrictHostKeyChecking no" root@"$host" "$command"
}

find_hosts() {
  local host=$1
  local file="/config/concord/config-local/concord.config"
  local command="cat $file | grep replica_host | grep -Eo '([0-9]{1,3}[\.]){3}[0-9]{1,3}'"
  # shellcheck disable=SC2207
  hosts=($(do_ssh "$host" "$command"))
  hosts=("${hosts[@]/127.0.0.1/$host}")
}

open_firewall() {
  local host=$1
  local command="iptables -A INPUT -p icmp --icmp-type echo-request -j ACCEPT"
  do_ssh "$host" "$command"
}

close_firewall() {
  local host=$1
  local command="iptables -D INPUT -p icmp --icmp-type echo-request -j ACCEPT"
  do_ssh "$host" "$command"
}

do_ping() {
  local host=$1
  local target=$2
  local command="ping -i .001 -q -c 1000 $target"
  local dir="raw/$host"
  mkdir -p "$dir"
  do_ssh "$host" "$command" >"$dir"/"$target"
}

mutual_pings() {
  local host=$1
  local targets=$2
  for target in $targets; do
    if [ "$target" != "$host" ]; then
      echo "Ping $host to $target"
      do_ping "$host" "$target"
    fi
  done
}

main() {
  local host=$1
  find_hosts "$host"

  for host in "${hosts[@]}"; do
    open_firewall "$host"
  done

  for host in "${hosts[@]}"; do
    mutual_pings "$host" "${hosts[*]}"
  done

  for host in "${hosts[@]}"; do
    close_firewall "$host"
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

main "$1"
