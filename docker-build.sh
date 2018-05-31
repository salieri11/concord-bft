#!/bin/bash
#
# This script builds three athena images, each with different private
# keys, suitable for using in a docker-compose script.

# First build the base image, including athena and the public config.
docker build . -t athenabase

# Now build each of the specific images
docker build . -f docker/Dockerfile-athena1 -t athena1
docker build . -f docker/Dockerfile-athena2 -t athena2
docker build . -f docker/Dockerfile-athena3 -t athena3
