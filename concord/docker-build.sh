#!/bin/bash
#
# This script builds three concord images, each with different private
# keys, suitable for using in a docker-compose script.

default_name="concord"
default_tag="latest"

# Before even trying, make sure submodules are present
if [ ! -e "submodules/concord-bft/README.md" ]; then
    echo "concord-bft submodule not initialized."
    echo "Please run the following before building docker images:"
    echo "   git submodule init && git submodule update --recursive"
    exit 1
fi

if [ ! -e "submodules/state-transfer/README.md" ]; then
    echo "state-transfer module not initialized"
    echo "Please run the following before building docker images:"
    echo "   git clone https://github.com/vmwathena/state-transfer.git"
    exit 1
fi

if [ $# -eq 2 ]; then
    name="${1}"
    tag="${2}"
elif [ $# -eq 0 ]; then
    name="${default_name}"
    tag="${default_tag}"
else
    echo Usage: docker-build.sh [name tag]
    echo If the name and tag are not provided, the defaults are:
    echo name: "${default_name}"
    echo tag: "${default_tag}"
    exit 1
fi

# Build the generic image. See docker/docker-compose.yml for how to
# launch this image for each replica configuration.
docker build . -t "${name}:${tag}"

if [ $? -ne 0 ]; then
    echo "Image creation failed."
    exit 1
fi
