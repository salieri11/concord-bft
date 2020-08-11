#!/bin/bash
# set -x

BLOCKCHAIN_DIR=`dirname "$(readlink -f $0)"`
EVENTS_FILE="${BLOCKCHAIN_DIR}/../times.json"
EVENTS_RECORDER="${BLOCKCHAIN_DIR}/hermes/event_recorder.py"
tag=${BUILD_NUMBER}
repo=$(release_repo) + "/"

# buildlib.sh contains code which manages the build processes, retries,
# determining success/failure, etc...
# TODO remove
. hermes/lib/shell/buildlib.sh

# A sanity check for Docker. Exits if not present.
verifyDocker(){
  DOCKER=`which docker`

  if [ "${DOCKER}" == "" ]
  then
    error "Docker was not found on this system."
    exit 1
  else
    info "Docker: ${DOCKER}"
  fi
}

docker_pull() {
    if [ "$#" -ne "2" ]
    then
        error "Missing Required Parameters for docker pull method"
        error "Usage: docker_pull <docker image:tag> <Task Name>"
        killAllProcs
        exit 1
    fi
    local DOCKER_IMAGE_WITH_TAG="$1"
    local DOCKER_PULL_TASK_NAME="$2"
    local LOG_FILE="${DOCKER_PULL_TASK_NAME}.log"

    docker pull "${DOCKER_IMAGE_WITH_TAG}" > "${LOG_FILE}" 2>&1 &
    addToProcList "${DOCKER_PULL_TASK_NAME}" $! "${LOG_FILE}"
}

docker_build() {
    if [ "$#" -lt "4" ]
    then
        error "Missing Required Parameters for docker build method"
        error "Usage: docker_build <directory to docker build> <docker build file> <docker build name> <docker build tag> [<optional parmeters like --build-arg \"key=val\">]"
        killAllProcs
        exit 1
    fi
    local DOCKER_BUILD_DIR="$1"
    shift
    local DOCKER_BUILD_FILE="$1"
    shift
    local DOCKER_REPO_NAME="$1"
    shift
    local DOCKER_REPO_TAG="$1"
    shift

    info "DOCKER_REPO_TAG: ${DOCKER_REPO_TAG}"

     local BUILD_ARG_PARAM=""
     while [[ "$1" != "" ]] ; do
        case $1 in
           "--build-arg")
              shift
              BUILD_ARG_PARAM+=" --build-arg $1"
              ;;
        esac
        shift
      done

      local LOG_FILE=`basename "${DOCKER_REPO_NAME}"_build.log`
      docker build "${DOCKER_BUILD_DIR}" -f "${DOCKER_BUILD_FILE}" -t "${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG}" --label ${version_label}=${DOCKER_REPO_TAG} --label ${commit_label}=${commit_hash} ${BUILD_ARG_PARAM} > "${LOG_FILE}" 2>&1 &
      addToProcList `basename "${DOCKER_REPO_NAME}_image"` $! "${LOG_FILE}"
}

node-dependency() {
    info "Installing node package dependencies..."
    . ~/.nvm/nvm.sh
    nvm install 11.15.0
    nvm alias default 11.15.0
    npm_install "node dependency for UI" "ui"
    npm_install "node dependency for contract-compiler" "contract-compiler"
}

npm_install() {
  NAME="${1}"
  COMPONENT_DIR="${2}"

  pushd .
  cd "$COMPONENT_DIR"
  local LOG_FILE="node_install_${COMPONENT_DIR}.log"
  npm config set registry https://build-artifactory.eng.vmware.com/artifactory/api/npm/npm
  npm install > "${LOG_FILE}" 2>&1 &
  NODE_BUILD_PID=$!
  addToProcList "${NAME}" $! "${LOG_FILE}"
  waitForProcesses
  popd
}

ui() {
    info "Build UI..."
    docker_build ui ui/Dockerfile ui ${tag}
}

fluentd() {
    info "Build fluentd..."
    docker_build docker/fluentd docker/fluentd/Dockerfile fluentd ${tag}
}

helen() {
    info "Build helen..."
    docker_build . helen/Dockerfile helen ${tag}
}

persephone() {

    info "Build persephone..."
    docker_build persephone persephone/agent/Dockerfile agent ${tag}
    docker_build persephone persephone/ip-allocation-service/Dockerfile persephone-ipam ${tag}

    docker_build persephone persephone/provisioning-service/Dockerfile persephone-provisioning ${tag}

    # TODO Change to custom(static)
#    docker_build . persephone/config-service/Dockerfile persephone-configuration ${tag} --build-arg "concord_repo=concord-core" --build-arg "concord_tag=1446"
}

# TODO: Associative arrays don't work in OSX's default shell.
declare -A BUILD_PROCS

# Array of successful build targets.
SUCCESSES=()

info "Loading repos/tags for docker images from docker/.env"

version_label="com.vmware.blockchain.version"
commit_label="com.vmware.blockchain.commit"

verifyDocker

info "**** Building all components..."
node-dependency

ui
fluentd
helen
persephone
waitForProcesses

waitForProcesses
