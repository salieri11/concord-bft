#!/bin/bash

cd /home/blockchain/orchestrator-runtime
IP_ADDRESS=$(python3 -c 'import netifaces as nif; print(nif.ifaddresses("eth0")[2][0]["addr"], end="")')
logger "Starting VMware Blockchain Orchestrator configuration service with IP: $IP_ADDRESS"

# List out the configuration first
CONFIG_SERVICE_IP=$IP_ADDRESS docker-compose -f docker-compose-castor-prereqs.yml config

# Shut down any dangling containers
CONFIG_SERVICE_IP=$IP_ADDRESS docker-compose -f docker-compose-castor-prereqs.yml down --remove-orphans

# Start em up.
CONFIG_SERVICE_IP=$IP_ADDRESS docker-compose -f docker-compose-castor-prereqs.yml up -d
