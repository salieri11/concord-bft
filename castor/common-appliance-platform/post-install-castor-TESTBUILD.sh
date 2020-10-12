#!/bin/bash
set -x -v

#/* **************************************************************************** *
#* Copyright (c) 2020 VMware, Inc.  All rights reserved. -- VMware Confidential *
#* **************************************************************************** */

# This script is to be used for test builds for Automation/QE.
# It sets up automatic EULA acceptance so root/blockchain logins can be automated.
# To create a test build, set this in orchestrator-appliance-remote.json:
# 	"after-appliance-ready-script": "post-install-castor-TESTBUILD.sh"

# This is a copy of post-install-castor.sh, but CAP does not allow me to
# source this file since CAP does not transfer the files to the appliance.
# Hence the cut-n-paste.

# Run the production script

# blockchain user
BLOCKCHAIN_USER=blockchain
ONPREM_BLOCKCHAIN_ARTIFACTORY=athena-docker-local.artifactory.eng.vmware.com
CONFIG_SERVICE_NAME=persephone-configuration
PROVISIONING_SERVICE_NAME=persephone-provisioning
CASTOR_SERVICE_NAME=castor
ONPREM_BLOCKCHAIN_VERSION=0.0.0.2323

CONFIG_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CONFIG_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
PROVISIONING_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$PROVISIONING_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION
CASTOR_SERVICE=$ONPREM_BLOCKCHAIN_ARTIFACTORY/$CASTOR_SERVICE_NAME:$ONPREM_BLOCKCHAIN_VERSION

# This needs to match the /software/copy-to-appliance/misc-files/local/target value
# in the CAP appliance json. This is the location to which files are copied over from
# the build machine to the appliance. From here, they are distributed to various other
# appliance directories/files.
APPLIANCE_FILES=/root/blockchain

# This is needed to enable docker bridge networking. Ensure that the
# Kernel version on the base image matches this value.
LINUX_KERNEL_VERSION=4.19.129-1.ph3


VMWARE_BLOCKCHAIN_DIR=/opt/vmware/blockchain
mkdir -p $VMWARE_BLOCKCHAIN_DIR
chmod 755 $VMWARE_BLOCKCHAIN_DIR

# Add user blockchain
useradd -s /bin/bash -m -G docker $BLOCKCHAIN_USER

mkdir /home/$BLOCKCHAIN_USER/orchestrator-runtime
CASTOR_ARTIFACTS_DIR=/home/$BLOCKCHAIN_USER/orchestrator-runtime

# The base SH photon image does not have docker bridge enabled. Due to this the
# docker service fails to start. Enable it here.
# From:
# https://opengrok.eng.vmware.com/source/xref/main.perforce.1666/bora/install/vpxd/linux/bin/csgw.sh
# Remove "install bridge /bin/false" from modprobe config
echo "Enabling docker bridge networking: kernel version: $LINUX_KERNEL_VERSION"
grep -v 'install bridge' /etc/modprobe.d/modprobe.conf > /tmp/modprobe
mv /tmp/modprobe /etc/modprobe.d/modprobe.conf

if [ -d /usr/lib/modules/$LINUX_KERNEL_VERSION ]
then
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/llc/llc.ko.xz
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/802/stp.ko.xz
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/bridge/bridge.ko.xz

    mkdir /etc/docker
    echo '{"bip": "172.17.0.1/16"}' > /etc/docker/daemon.json

    # Start docker
    systemctl restart docker
    echo "Finished setting up docker bridge"
else
    echo "ERROR: Docker bridge: module directory /usr/lib/modules/$LINUX_KERNEL_VERSION not found"
    echo "ERROR: Docker bridge setup skipped: Docker service will not be available"
    exit -1
fi

# In build 16973301 of the SH appliance, the default python links to python2.
# The python2 invocation causes the ovfenv tool to fail, leading to firstboot problems.
# Remove this if the SH folks ever fix the problem.
pushd /usr/bin
rm python
ln -s python3 python
popd

# Install docker-compose
curl -L "https://github.com/docker/compose/releases/download/1.27.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod 755 /usr/local/bin/docker-compose

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

cp $APPLIANCE_FILES/orchestrator-prereqs-launcher.sh $VMWARE_BLOCKCHAIN_DIR
chmod 755 $VMWARE_BLOCKCHAIN_DIR/orchestrator-prereqs-launcher.sh

echo "Setting up the blockchain service"
cp $APPLIANCE_FILES/blockchain.service /lib/systemd/system
chmod 644 /lib/systemd/system/blockchain.service

ln -s /lib/systemd/system/blockchain.service /etc/systemd/system
systemctl daemon-reload
systemctl enable blockchain.service
# Make sure it is set up. This output goes to journal
systemctl status blockchain.service
# End blockchain service setup. This will be started by systemd when the system starts up.

# Set up first-boot actions. This is called via a service provided by CAP, to set parameters on deployed appliances,
# like enabling ssh, passwords, network configuration, etc. For now, we only do root and blockchain passwords.
# The CAP firstboot service checks for a file called /etc/vmware/cap/cap-firstboot.sh, which it calls at first boot.
mkdir -p /etc/vmware/cap
cp $APPLIANCE_FILES/cap-firstboot.sh /etc/vmware/cap/cap-firstboot.sh
chmod 755 /etc/vmware/cap/cap-firstboot.sh

# Set up EULA acceptance
# Create a directory where EULA acceptance will be recorded, with a file named .$USER-eula.
EULA_DIR=/opt/vmware/blockchain/eula
mkdir -p  $EULA_DIR
chmod 777 $EULA_DIR
rm -rf $EULA_DIR/*

# Copy the EULA artifacts to the appliance
cp $APPLIANCE_FILES/eula.script $EULA_DIR
chmod 555 $EULA_DIR/eula.script
cp $APPLIANCE_FILES/VMware_EULA_20190913_English.txt $EULA_DIR
chmod 444 $EULA_DIR/VMware_EULA_20190913_English.txt
# End EULA acceptance setup

# The last statement needs to return code 0 for CAP to assume success.
echo "post-install-castor.sh run finished"

# End production script

# Accept EULA for test/automation
echo "Running a TEST BUILD"
# Do what eula.script would have done if the user had accepted the EULA
echo "TEST BUILD: User root accepted EULA on $(date -u)" >> $EULA_DIR/.root-eula
echo "TEST BUILD: User blockchain accepted EULA on $(date -u)" >> $EULA_DIR/.blockchain-eula
