#!/bin/bash
#
# Usage: ./gen-docker-concord-config.sh config-public/dockerConfigurationInput.yaml

DOCKER_DIR=$(
  python -c 'import os; print(os.path.abspath(os.getcwd()))')

RUN_DIR="vmwathena_blockchain/docker"
if [[ "${DOCKER_DIR}" != *${RUN_DIR} ]]; then
  echo "Please run this script from inside \"${RUN_DIR}\""
  exit 1
fi

if [ $# -ne 1 ]; then
  echo "Please provide a configuration for the conc_genconfig tool"
  exit 1
fi

MOUNT_POINT="/dockerydoo"
DOCKER_IMAGE="$(grep '\bconcord_repo' .env | awk -F'=' '{print $2}'):$(grep '\bconcord_tag' .env | awk -F'=' '{print $2}')"

docker inspect ${DOCKER_IMAGE} > /dev/null 2>&1
if [ $? -ne 0 ]; then
  echo "Couldn't find \"${DOCKER_IMAGE}\" docker image. Either build the" \
       "image locally, or pull the Concord image from artifactory and" \
       "update your docker/.env file."
  exit 1
fi

docker run -it --rm \
  -v ${DOCKER_DIR}:${MOUNT_POINT} \
  ${DOCKER_IMAGE} \
  /concord/conc_genconfig --configuration-input ${MOUNT_POINT}/${1} \
                          --configuration-type application \
                          --output-name ${MOUNT_POINT}/config-public/application

docker run -it --rm \
  -v ${DOCKER_DIR}:${MOUNT_POINT} \
  ${DOCKER_IMAGE} \
  /concord/conc_genconfig --configuration-input ${MOUNT_POINT}/${1} \
                          --configuration-type deployment \
                          --output-name ${MOUNT_POINT}/config-public/deployment

docker run -it --rm \
  -v ${DOCKER_DIR}:${MOUNT_POINT} \
  ${DOCKER_IMAGE} \
  /concord/conc_genconfig --configuration-input ${MOUNT_POINT}/${1} \
                          --configuration-type secrets \
                          --output-name ${MOUNT_POINT}/config-public/secrets

${DOCKER_DIR}/config-public/distribute-configuration-files.sh
