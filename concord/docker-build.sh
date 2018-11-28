#!/bin/bash
#
# This script builds three concord images, each with different private
# keys, suitable for using in a docker-compose script.

default_name="concord"
default_tag="latest"
label_string=""

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

echo "${1}" | grep -i "help" > /dev/null
need_help=$?

if [ $# -eq 0 ]
then
    name="${default_name}"
    tag="${default_tag}"
elif [ $# -eq 1 ]
then
    need_help=0
elif [ $# -ge 2 ]; then
    name="${1}"
    shift
    tag="${1}"
    shift
fi

if [ "${need_help}" -eq 0 ]
then
    echo Usage: docker-build.sh name tag [labelKey=labelValue labelKey=labelValue ...]
    echo If the name and tag are not provided, the defaults are:
    echo name: "${default_name}"
    echo tag: "${default_tag}"
    exit 1
fi

for label in "$@"
do
    label_string="${label_string}--label \"${label}\" "
done

full_command="docker build . -t \"${name}:${tag}\""

if [ "${label_string}" != "" ]
then
    full_command="${full_command} ${label_string}"
fi

echo "Building docker image with command: ${full_command}"
eval "${full_command}"

if [ $? -ne 0 ]; then
    echo "Image creation failed."
    exit 1
fi
