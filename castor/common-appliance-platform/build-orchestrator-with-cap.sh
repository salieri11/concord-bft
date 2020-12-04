#!/usr/bin/env bash

# This script sets up a CAP environemt (CAP+packer+ovf binaries) to build the
# Orchestrator appliance OVA.
# The instructions have been copied from:
# https://confluence.eng.vmware.com/display/CPDVC/CAP+-+Getting+Started

# Once the CAP environment is set up, the script triggers off a build for the Orchestrator OVA

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PREREQUISITE: BEFORE RUNNING THIS SCRIPT: Update build.properties with the correct values.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Substitute placeholders in orchestrator-appliance-remote.json
source ./build.properties
sed -i "s@<ORCHESTRATOR_APPLIANCE_NAME>@${ORCHESTRATOR_APPLIANCE_NAME}@" orchestrator-appliance-remote.json
sed -i "s@<ORCHESTRATOR_BUILD_FILES_REPOSITORY>@${ORCHESTRATOR_BUILD_FILES_REPOSITORY}@" orchestrator-appliance-remote.json
sed -i "s@<ORCHESTRATOR_CAP_BASE_IMAGE>@${ORCHESTRATOR_CAP_BASE_IMAGE}@" orchestrator-appliance-remote.json
sed -i "s@<ORCHESTRATOR_BUILD_VCENTER>@${ORCHESTRATOR_BUILD_VCENTER}@" orchestrator-appliance-remote.json
sed -i "s@<ORCHESTRATOR_BUILD_HOST>@${ORCHESTRATOR_BUILD_HOST}@" orchestrator-appliance-remote.json


# Prepare CAP environment
# Download the CAP binaries
rm -rf ./cap-lin*.zip ./cap ./.cap ./.input ./input.json ./.scripts ./ovftool* ./packer ./packer_cache ./template.json
rm -rf images/* logs/*

wget $ORCHESTRATOR_CAP_BINARY
unzip cap-lin.zip

mv ovftool ovftool4.4.0
ln -s ./ovftool4.4.0/ovftool ovftool

export PATH=$PWD:$PATH

# Initialize CAP
./cap init

# Trigger off a build of the OVA. The image will be published under ./images

./cap build -f orchestrator-appliance-remote.json
