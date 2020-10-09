#!/bin/bash
#
# This script is for distributing Concord configuration files in the
# Docker-based test cluster; this script is intended to copy the configuration
# files generated by the Concord configuration generation utility's container
# to the environments in which the Concord nodes' containers run. This script
# takes arguments for the sources and destinations of the configuration files to
# move; if no arguments are provided, the default values this script uses are
# such that, if this script is launched from the vmathena_blockchain/docker
# directory, it should move the configuration files generated for the 4-node
# Concord test cluster in Docker.
#
# Arguments:
#  1:   Path and prefix common to the configuration files as the configuration
#       generation utility has produced them; it is expected the files will have
#       names <PREFIX>1.config, <PREFIX>2.config, etc. For example, if <PREFIX>
#       is ./config-public/concord, this script will look for the configuration
#       files at ./config-public/concord1.config,
#       ./config-public/concord2.config, etc.
#  2-N: Target paths to move the configuration files to.

if [[ $# -gt 0 ]]; then
    PUBPATH=$1
    shift
else
    PUBPATH="./config-public/"
fi

if [[ $# -gt 0 ]]; then
    PRIVPATHS=("$@")
else
    # Default
    PREFIX="./config-concord"
    for f in $(ls config-public/application*.config); do
        # Get the replica number
        NUM=${f%.config}
        NUM=${NUM##*/application}

        # Create destination directory
        mkdir -p ${PREFIX}${NUM}

        PRIVPATHS+=("${PREFIX}${NUM}")
    done
fi

function getFileName() {
    echo "/$1$2.config"
}

function distributionFailure() {
    >&2 echo "WARNING! Can't distribute config file for node $1. Check if $2 exists in ${PUBPATH}."
}

for i in $(seq 1 ${#PRIVPATHS[@]}); do

    mv -f ${PUBPATH}$(getFileName application $i) ${PRIVPATHS[i - 1]}$(getFileName application) || distributionFailure $i $(getFileName application $i)
    mv -f ${PUBPATH}$(getFileName deployment $i) "${PRIVPATHS[i - 1]}$(getFileName deployment)" || distributionFailure $i $(getFileName deployment $i)
    mv -f ${PUBPATH}$(getFileName secrets $i) "${PRIVPATHS[i - 1]}$(getFileName secrets)" || distributionFailure $i $(getFileName secrets $i)
done
