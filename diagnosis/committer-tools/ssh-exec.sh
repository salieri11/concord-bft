#!/bin/bash

function ssh_exec {
  if [[ $# -lt 2 ]]; then
    >&2 echo "ssh_exec <IP> <COMMAND>"
    exit 1
  fi
  if [[ ! -z $1 ]]; then
    IP=$1; shift
  else
    >&2 echo "IP address required"
    exit 1
  fi
  if [[ -z $@ ]]; then
    >&2 echo "No command specified"
    exit 1
  fi
  if [[ -z ${SSHPASS} ]]; then
    PREFIX=""
  else
    PREFIX="sshpass -e"
  fi
  ${PREFIX} ssh root@${IP} -oStrictHostKeyChecking=no -oConnectTimeout=10 "$@"
}

function ssh_exec_script {
  if [[ $# -ne 2 ]]; then
    >&2 echo "ssh_exec_script <IP> <PATH_TO_SCRIPT>"
    exit 1
  fi
  if [[ -z $1 ]]; then
    >&2 echo "IP address required"
    exit 1
  else
    IP=$1
  fi
  if [[ -z $2 ]]; then
    >&2 echo "No script specified"
    exit 1
  fi
  if [[ ! -f $2 ]]; then
    >&2 echo "Script '$2' doesn't exist"
    exit 1
  fi
  if [[ -z ${SSHPASS} ]]; then
    PREFIX=""
  else
    PREFIX="sshpass -e"
  fi
  ${PREFIX} ssh root@${IP} -T -oStrictHostKeyChecking=no -oConnectTimeout=10 < "$2"
}

[[ ${BASH_SOURCE[0]} = $0 ]] && ssh_exec $@
