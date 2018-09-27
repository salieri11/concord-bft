#!/bin/bash
#
# This script builds three athena images, each with different private
# keys, suitable for using in a docker-compose script.

# Before even trying, make sure submodules are present
if [ ! -e "submodules/P2_Blockchain/README.md" ]; then
    echo "P2_Blockchain submodule not initialized."
    echo "Please run the following before building docker images:"
    echo "   git submodule init && git submodule update --recursive"
    exit 1
fi

# Default tag is "latest".
if [ $# -gt 0 ]; then
	tag=":$1"
fi

# Build the generic image. See docker/docker-compose.yml for how to
# launch this image for each replica configuration.
docker build . -t athena$tag

if [ $? -ne 0 ]; then
    echo "Image creation failed."
    exit 1
fi
