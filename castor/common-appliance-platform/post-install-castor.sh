#!/bin/bash
set -x -v

#/* **************************************************************************** *
#* Copyright (c) 2020 VMware, Inc.  All rights reserved. -- VMware Confidential *
#* **************************************************************************** */
# This script does post install tasks for the blockchain appliance.

# blockchain user
BLOCKCHAIN_USER=blockchain
ONPREM_BLOCKCHAIN_ARTIFACTORY=athena-docker-local.artifactory.eng.vmware.com
CONFIG_SERVICE_NAME=persephone-configuration
PROVISIONING_SERVICE_NAME=persephone-provisioning
CASTOR_SERVICE_NAME=castor
ONPREM_BLOCKCHAIN_VERSION=0.0.0.2252

CONFIG_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CONFIG_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
PROVISIONING_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$PROVISIONING_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
CASTOR_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CASTOR_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION

# This needs to match the /software/copy-to-appliance/misc-files/local/target value in the CAP appliance json
APPLIANCE_FILES=/root/blockchain

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

# Change permissions  to their correct values
chmod -R 744 $CASTOR_ARTIFACTS_DIR
chown -R $BLOCKCHAIN_USER:users /home/$BLOCKCHAIN_USER

# Set up the systemd service to launch the docker-compose-orchestrator-prereqs.yml file.
# This launch requires the IP address of the appliance that it is running on (CONFIG_SERVICE_IP). So it
# is set up and launched as a systemd service.

mkdir -p /opt/vmware/blockchain
cp $APPLIANCE_FILES/orchestrator-prereqs-launcher.sh /opt/vmware/blockchain
chmod 744 /opt/vmware/blockchain/orchestrator-prereqs-launcher.sh

cp $APPLIANCE_FILES/blockchain.service /lib/systemd/system
chmod 644 /lib/systemd/system/blockchain.service

ln -s /lib/systemd/system/blockchain.service /etc/systemd/system

systemctl daemon-reload

systemctl enable blockchain.service

# Make sure it is set up
systemctl status blockchain.service

# The last statement needs to return code 0 for CAP to assume success.
echo "Finished setting up the castor blockchain prerequisite service"

# End blockchain service setup. This will be started by systemd when the system starts up.

