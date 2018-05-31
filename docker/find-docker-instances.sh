#!/bin/bash
#
# This script is for running Athena under Docker Compose. The SBFT
# config file currently requires an IP to be specified at start
# time. We have to wait for Docker to assign those IPs first. Thus,
# this script waits for IPs of the athena nodes, and then insert them
# into the public SBFT config file.
#
# Arguments:
#  1: The path to the public config
#  2-N: The names of the hosts to find IPs for.

PUBPATH=$1
shift

while [[ $# -gt 0 ]]; do
    HOSTNAME=$1

    # wait for IP address to be available
    host $HOSTNAME > /dev/null
    while [[ $? -ne 0 ]]; do
          echo "Waiting on ${HOSTNAME}"
          host $HOSTNAME > /dev/null
          sleep 1
    done

    # extract IP address
    IPADDRESS=`host ${HOSTNAME} | awk '{print $NF}' | head -1`
    echo "Found ${HOSTNAME} at $IPADDRESS"

    # put IP address in
    sed -i -e "s/${HOSTNAME} [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/${HOSTNAME} ${IPADDRESS}/" $PUBPATH

    shift
done
