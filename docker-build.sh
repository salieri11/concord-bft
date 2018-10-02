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

# First build the base image, including athena and the public config.
docker build . -t athenabase

if [ $? -ne 0 ]; then
    echo "Base image creation failed. Exiting before specialization."
    exit 1
fi

if [ $# -gt 0 ]; then
	tag=":$1"
fi
# Now build each of the specific images
docker build . -f docker/Dockerfile-athena1 -t athena1$tag
docker build . -f docker/Dockerfile-athena2 -t athena2$tag
docker build . -f docker/Dockerfile-athena3 -t athena3$tag
docker build . -f docker/Dockerfile-athena4 -t athena4$tag
