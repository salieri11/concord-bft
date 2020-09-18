#!/bin/bash
set -x -v

#/* **************************************************************************** *
#* Copyright (c) 2020 VMware, Inc.  All rights reserved. -- VMware Confidential *
#* **************************************************************************** */
# This script does post processing task

# blockchain user
BLOCKCHAIN_USER=blockchain
ONPREM_BLOCKCHAIN_ARTIFACTORY=athena-docker-local.artifactory.eng.vmware.com
CONFIG_SERVICE_NAME=persephone-configuration
PROVISIONING_SERVICE_NAME=persephone-provisioning
CASTOR_SERVICE_NAME=castor
ONPREM_BLOCKCHAIN_VERSION=0.0.0.2170

CONFIG_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CONFIG_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
PROVISIONING_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$PROVISIONING_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
CASTOR_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CASTOR_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION

# Add user blockchain
useradd -s /bin/bash -m -G docker $BLOCKCHAIN_USER

mkdir /home/$BLOCKCHAIN_USER/orchestrator-runtime
CASTOR_ARTIFACTS_DIR=/home/$BLOCKCHAIN_USER/orchestrator-runtime

# Install docker-compose
curl -L "https://github.com/docker/compose/releases/download/1.27.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Add the docker containers
docker pull $CONFIG_SERVICE
docker pull $PROVISIONING_SERVICE
docker pull $CASTOR_SERVICE

# Copy the docker-compose yml files and supporting scripts (wait-for-it.sh, etc. etc)
# from the castor container to the host, so it can be used by docker-compose.

# Need to create a different container over the original one
docker create --name castor-compose-artifacts $CASTOR_SERVICE bash
docker cp castor-compose-artifacts:/orchestrator-runtime /home/$BLOCKCHAIN_USER
# Delete the temporary container after use
docker rm castor-compose-artifacts

# Create a .env file so that the correct versions of the docker images are picked up.
# These should match the variables specified in the docker-compose files under /orchestrator-runtime.
cat > $CASTOR_ARTIFACTS_DIR/.env <<EOF
persephone_provisioning_repo=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$PROVISIONING_SERVICE_NAME
persephone_provisioning_tag=$ONPREM_BLOCKCHAIN_VERSION
persephone_configuration_repo=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CONFIG_SERVICE_NAME
persephone_configuration_tag=$ONPREM_BLOCKCHAIN_VERSION
castor_repo=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CASTOR_SERVICE_NAME
castor_tag=$ONPREM_BLOCKCHAIN_VERSION
EOF

# Get the IP address of this appliance. It is required for startring up the config service container,
# so that the agent can talk to the config service running on this appliance.
IP_ADDRESS=$(python3 -c 'import netifaces as nif; print(nif.ifaddresses("eth0")[2][0]["addr"], end="")')
echo "Config Service IP address: " $IP_ADDRESS

# Change permissions  to their correct values
chmod -R 744 $CASTOR_ARTIFACTS_DIR
chown -R $BLOCKCHAIN_USER:users /home/$BLOCKCHAIN_USER

# Launch the prereqs file: This launches the provisioning and config services, and keeps them
# around for as long as the appliance is up.
pushd $CASTOR_ARTIFACTS_DIR
CONFIG_SERVICE_IP=$IP_ADDRESS docker-compose -f docker-compose-castor-prereqs.yml up -d
popd

