#!/bin/bash
#
# This script is for running concord under Docker Compose. The SBFT
# config file currently requires an IP to be specified at start
# time. We have to wait for Docker to assign those IPs first. Thus,
# this script waits for IPs of the concord nodes, and then insert them
# into the public SBFT config file.
#
# Arguments:
#  1: The path to the public config
#  2-N: The names of the hosts to find IPs for.

if [[ $# -gt 0 ]]; then
    PUBPATH=$1
    shift
else
    PUBPATH=/concord/config-local/s_f1c0_config.pub
fi

if [[ $# -gt 0 ]]; then
    HOSTNAMES=$@
else
    HOSTNAMES=("concord1" "concord2" "concord3" "concord4")
fi

for HOSTNAME in ${HOSTNAMES[@]}; do
    # wait for IP address to be available
    while ! getent ahostsv4 $HOSTNAME > /dev/null; do
        echo "Waiting for ${HOSTNAME}"
        sleep 1
    done

    # extract IP address
    IPADDRESS=`getent ahostsv4 ${HOSTNAME} | head -1 | awk '{print $1}'`
    echo "Found ${HOSTNAME} at $IPADDRESS"

    # put IP address in
    sed -i -e "s/${HOSTNAME} [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/${HOSTNAME} ${IPADDRESS}/" $PUBPATH
done
