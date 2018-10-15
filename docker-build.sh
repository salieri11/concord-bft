#!/bin/bash
#
# This script builds three athena images, each with different private
# keys, suitable for using in a docker-compose script.

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

# The tag is passed in by docker-build.sh.  If run by other
# means, the tag will be athena@latest.
if [ $# -gt 0 ]; then
    tag="${1}"
else
    tag="athena:latest"
fi

# Build the generic image. See docker/docker-compose.yml for how to
# launch this image for each replica configuration.
docker build . -t "${tag}"

if [ $? -ne 0 ]; then
    echo "Image creation failed."
    exit 1
fi
