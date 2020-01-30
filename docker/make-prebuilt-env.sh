#!/bin/bash

# Create a .env file that points to the latest builds on artifactory.
#
# Your Artifactory API key must be set in the environment variable
# $ARTIFACTORY_KEY. To get your key, log into Artifactory and visit
# your profile here:
# https://build-artifactory.eng.vmware.com/artifactory/webapp/#/profile
#
# Unlock your profile, then copy your API Key from the field on the
# page (generate a new one if you don't have one yet), then set it in
# your terminal:
#
#    export ARTIFACTORY_KEY=<paste your key here>
#
# By default, the script reads from the file named ".env". To read
# from a different source file, pass the filename as the first
# argument to the script.
#
# The script prints a definition using the latest prebuilt images to
# stdout. To update your .env "in place", use the following:
#
#    ./make-prebuilt-env.sh > temp.env && mv temp.env .env

if [ -z ${ARTIFACTORY_KEY} ]; then
    echo "No ARTIFACTORY_KEY found. Please specify per instructions."
    exit -1
fi

if [ -z $1 ]; then
    SOURCE_FILE=".env"
else
    SOURCE_FILE=$1
fi

ARTIFACTORY_BASE_URL="https://build-artifactory.eng.vmware.com/api/storage/athena-docker-local"
ARTIFACTORY_BASE_IMAGE_PATH="athena-docker-local.artifactory.eng.vmware.com/"

# Find the last athena-docker-local change and extract the tag from it:
#   1. curl output is JSON containing https://.../athena-docker-local/<some image>/<TAG>/<maybe more stuff>
#   2. First grep extracts https://.../<TAG> (dropping /<maybe more stuff>)
#   3. Second grep extracts <TAG> (droping https://.../)
LATEST_TAG=$(curl -s -H "X-JFrog-Art-Api: ${ARTIFACTORY_KEY}" ${ARTIFACTORY_BASE_URL}/ethrpc?lastModified |
                   perl -ne 'print $1 if /\/([a-f0-9\.]+)\//')

while IFS= read -r LINE
do
    if [[ $LINE =~ repo ]]; then
        # Ex: concord_repo=persephone
        #     ^-----^      ^----------^
        #     SERVICE      IMAGE_BASE_NAME
        SERVICE_REPO=`echo ${LINE} | cut -d "=" -f 1 -`
        SERVICE=${SERVICE_REPO%_repo}
        IMAGE_BASE_NAME=`echo ${LINE} | cut -d "=" -f 2 -`

        if [[ ${SERVICE} == daml_ledgerapi ]]; then
            continue
        fi

        # try not to double-up artifactory prefixes if someone runs this twice
        if [[ $IMAGE_BASE_NAME =~ $ARTIFACTORY_BASE_IMAGE_PATH ]]; then
            IMAGE_BASE_NAME=`echo ${IMAGE_BASE_NAME} | cut -d "/" -f 2 -`
        fi

        echo "${SERVICE}_repo=${ARTIFACTORY_BASE_IMAGE_PATH}${IMAGE_BASE_NAME}"
        echo "${SERVICE}_tag=${LATEST_TAG}"
    fi
done < $SOURCE_FILE
