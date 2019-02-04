#!/bin/bash
#
# This script is for running Concord in Kuberbetes. The SBFT
# config file currently requires an IP to be specified at start
# time. With K8s, these IP:port settings are exposed via service
# objects. We have to wait for K8s to assign those IPs first. Thus,
# this script waits for IPs of the concord nodes, and then insert them
# into the public SBFT config file.
#
# Arguments:
#  1: The path to the public config file.
#  2: Placeholder name to be replaced.
#  3: Kubernetes service name to replace placeholder with.
CONFIGPATH=$1
PLACEHOLDER=$2
NODENAME=$3

until nslookup ${NODENAME}; do
    echo "Waiting for ${NODENAME} service"
    sleep 2
done

# Call nslookup, skip first 2 lines (about DNS server),
# grep for "Address" and extract output
IPADDRESS=$(nslookup ${NODENAME} | tail -n +3 | grep Address | cut -d" " -f 2)
sed -i -e "s/${PLACEHOLDER} [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/${PLACEHOLDER} ${IPADDRESS}/" ${CONFIGPATH}
