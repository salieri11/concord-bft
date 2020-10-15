#!/bin/bash
# set -x

#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# High level script to build the blockchain components.
# To add a component:
#   Go to the commands at the end of the script.
#   Add a line to start building the component in a background process:
#     <command to build foo> &
#   Add a line to store that background process in the process list:
#     addToProcList "Foo" $! 
#
# To build OPTIONAL/ADDITIONAL components (that are not part of the default build),
#   Execute: ./buildall.sh --additionalBuilds <function_name1,function_name2>
#   Example: ./buildall.sh --additionalBuilds PerformanceTests,StressTests
################################################################################

# For saveTimeEvent
. hermes/lib/shell/saveTimeEvent.sh
BLOCKCHAIN_DIR=`dirname "$(readlink -f $0)"`
EVENTS_FILE="${BLOCKCHAIN_DIR}/../times.json"
EVENTS_RECORDER="${BLOCKCHAIN_DIR}/hermes/event_recorder.py"

# buildlib.sh contains code which manages the build processes, retries,
# determining success/failure, etc...
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

PerformanceTests() {
    # Ivoked via --additionalBuilds PerformanceTests.
    cd performance/benchmark
    sh ./build.sh > performance_test_build.log 2>&1
}

BuildPersephoneGRPCpyBindings() {
    local LOG_FILE="${persephone_generate_grpc_bindings.log}"
    pushd .
    ./hermes/util/generate_grpc_bindings.py --source-path=persephone/api/src/protobuf --target-path=hermes/lib/persephone > "${LOG_FILE}"  2>&1 &
    addToProcList "Persephone gRPC Python Bindings" $! "${LOG_FILE}"
    popd
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
    info "PRODUCT_VERSION: ${PRODUCT_VERSION}"

    if [ "${DOCKER_REPO_TAG}" == "${PRODUCT_VERSION}" ]
    then
      local BUILD_ARG_PARAM=""
      local BUILD_LABEL_PARAM=""
      while [ "$1" != "" ] ; do
         case $1 in
            "--build-arg")
               shift
               BUILD_ARG_PARAM+=" --build-arg $1"
               ;;
            "--label")
               shift
               BUILD_LABEL_PARAM+=" --label $1"
               ;;
         esac
         shift
      done

      local LOG_FILE=`basename "${DOCKER_REPO_NAME}"_build.log`
      docker build "${DOCKER_BUILD_DIR}" -f "${DOCKER_BUILD_FILE}" -t "${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG}" --label ${version_label}=${DOCKER_REPO_TAG} --label ${commit_label}=${commit_hash} ${BUILD_ARG_PARAM} ${BUILD_LABEL_PARAM} > "${LOG_FILE}" 2>&1 &
      addToProcList `basename "${DOCKER_REPO_NAME}_image"` $! "${LOG_FILE}"
    else
      # When a component in .env has a different tag than PRODUCT_VERSION,
      # that means we pull it from artifactory instead of building it.
      local LOG_FILE=`basename "${DOCKER_REPO_NAME}"_pull.log`
      echo Pulling ${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG} instead of building it.
      docker pull ${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG} > "${LOG_FILE}" 2>&1 &
      addToProcList `basename "${DOCKER_REPO_NAME}_image"` $! "${LOG_FILE}"
    fi
}

docker_pull() {
    if [ "$#" -ne "2" ]
    then
        error "Missing Required Parameters for docker pull method"
        error "Usage: docker_pull <image name> <image tag>"
        killAllProcs
        exit 1
    fi
    DOCKER_REPO_NAME="$1"
    DOCKER_REPO_TAG="$2"
    local LOG_FILE=`basename "${DOCKER_REPO_NAME}"_pull.log`
    echo Pulling ${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG}
    docker pull ${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG} > "${LOG_FILE}" 2>&1 &
    addToProcList `basename "${DOCKER_REPO_NAME}_image"` $! "${LOG_FILE}"
}

node-dependency() {
    info "Installing node package dependencies..."
    saveTimeEvent "Install node dependencies" Start
    . ~/.nvm/nvm.sh
    nvm install 12.16.2
    nvm alias default 12.16.2
    npm_install "node dependency for UI" "ui"
    npm_install "node dependency for contract-compiler" "contract-compiler"
    saveTimeEvent "Install node dependencies" End
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

concord() {
    info "Build concord..."
    docker_build . concord/Dockerfile ${concord_repo} ${concord_tag}
}

memleak_concord() {
    info "Build concord for memoryleak..."
    docker_build . concord/DockerfileMemleak ${memleak_concord_repo} ${memleak_concord_tag} --build-arg "concord_repo=${concord_repo}" --build-arg "concord_tag=${concord_tag}"
}

ui() {
    info "Build UI..."
    docker_build ui ui/Dockerfile ${ui_repo} ${ui_tag}
}

fluentd() {
    info "Build fluentd..."
    docker_build persephone/fluentd persephone/fluentd/Dockerfile ${fluentd_repo} ${fluentd_tag}
}

ethereum() {
    info "Build ethereum..."
    docker_build . ethrpc/Dockerfile ${ethrpc_repo} ${ethrpc_tag}
}

helen() {
    info "Build helen..."
    docker_build . helen/Dockerfile ${helen_repo} ${helen_tag}
}

castor() {
    info "Build castor..."
    docker_build . castor/Dockerfile ${castor_repo} ${castor_tag}
}

agent() {
    info "Build persephone agent..."
    docker_build persephone persephone/agent/Dockerfile ${persephone_agent_repo} ${persephone_agent_tag}
}

persephone() {
    # As a precondition to building Persephone with this function, the Concord
    # image ${concord_repo}:${concord_tag} should be complete and available, and
    # should be the version of Concord that Persephone's configuration service
    # image ${persephone_configuration_repo}:${persephone_configuration_tag}
    # targets deploying. ${concord_repo}:${concord_tag} must be ready before
    # persephone() is called because a binary for Concord's configuration
    # generation utility will be copied into the image for Persephone's
    # configuration service in the build process.

    info "Build persephone..."
    docker_build persephone persephone/agent/Dockerfile ${persephone_agent_repo} ${persephone_agent_tag}

    # docker_build persephone persephone/ip-allocation-service/Dockerfile ${persephone_ipam_repo} ${persephone_ipam_tag}
    waitForProcesses

    docker_build persephone persephone/provisioning-service/Dockerfile ${persephone_provisioning_repo} ${persephone_provisioning_tag}
    docker_build . persephone/config-service/Dockerfile ${persephone_configuration_repo} ${persephone_configuration_tag} --build-arg "concord_repo=${concord_repo}" --build-arg "concord_tag=${concord_tag}"
}

reverse-proxy() {
    info "Pulling reverse-proxy..."
    docker_pull athena-docker-local.artifactory.eng.vmware.com/reverse-proxy "0.1.2"
}

asset-transfer() {
    info "Pulling asset-transfer..."
    # Asset transfer image will only be used prebuilt.
    docker_pull athena-docker-local.artifactory.eng.vmware.com/asset-transfer "2020.8.26"
}

supply-chain() {
    info "Pulling supply-chain..."
    docker_pull athena-docker-local.artifactory.eng.vmware.com/mrharrison/supply-chain "2020.8.26"
    waitForProcesses # docker_pull is async; below re-tagging cmd will not work if it's not awaited.
    docker tag athena-docker-local.artifactory.eng.vmware.com/mrharrison/supply-chain:2020.8.26 mrharrison/supply-chain:latest
}

contract-compiler() {
    info "Build contract-compiler..."
    docker_build contract-compiler contract-compiler/Dockerfile ${contract_compiler_repo} ${contract_compiler_tag}
}

daml() {
    info "Build DAML..."
    DAMLSDKVERSION=$(sed -n -e '/sdkVersion/ s/.*\= *//p' daml/build.sbt | tr -d '"')
    docker_build . daml/DockerfileLedgerApi ${daml_ledger_api_repo} ${daml_ledger_api_tag} --build-arg "bftclient_lib_repo=${participant_lib_repo}" --build-arg "bftclient_lib_tag=${participant_lib_tag}" --label ${daml_sdk_label}=$DAMLSDKVERSION
    docker_build . daml/DockerfileExecutionEngine ${daml_execution_engine_repo} ${daml_execution_engine_tag}
    docker_build . daml/DockerfilePostgres ${daml_index_db_repo} ${daml_index_db_tag}
}

trc-lib() {
    info "Build Thin Replica Client Library..."
    docker_build . thin-replica-client/Dockerfile ${trc_lib_repo} ${trc_lib_tag}
}

client-pool-lib() {
    info "Build BFT Client Pool Library..."
    docker_build . concord/src/external_client/DockerfileExternal ${client_pool_lib_repo} ${client_pool_lib_tag}
}

trc-test-app() {
    info "Build Thin Replica Client Test Application..."
    docker_build . thin-replica-client/DockerfileTestApp ${trc_test_app_repo} ${trc_test_app_tag} --build-arg "trc_lib_repo=${trc_lib_repo}" --build-arg "trc_lib_tag=${trc_lib_tag}"
}

participant-lib() {
    info "Build BFT Client Pool Library and Thin Replica Client..."
    docker_build . concord/src/external_client/DockerfileCombined ${participant_lib_repo} ${participant_lib_tag}  --build-arg "ext_lib_repo=${client_pool_lib_repo}" --build-arg "ext_lib_tag=${client_pool_lib_tag}" --build-arg "trc_lib_repo=${trc_lib_repo}" --build-arg "trc_lib_tag=${trc_lib_tag}"
}

PerformanceTests() {
    pushd .
    cd performance/benchmark
    sh ./build.sh > performance_test_build.log 2>&1
    popd
}

BuildPersephoneGRPCpyBindings() {
    pushd .
    ./hermes/util/generate_grpc_bindings.py --source-path=persephone/api/src/protobuf --target-path=hermes/lib/persephone > persephone_generate_grpc_bindings.log 2>&1 &
    addToProcList "Persephone gRPC Python Bindings" $!
    popd
}

# TODO: Associative arrays don't work in OSX's default shell.
declare -A BUILD_PROCS

# Array of successful build targets.
SUCCESSES=()

while [ "$1" != "" ] ; do
   case $1 in
      "--additionalBuilds")
         shift
         ADDITIONAL_BUILDS="$1"
         ;;
      "--buildOnDemand")
         shift
         COMPONENTS_TO_BUILD_ON_DEMAND="$1"
         ;;
   esac
   shift
done

DEPENDENCY_EXTERNAL_COMPONENTS="googletest evmjit ethereum_tests"
for component_name in `echo "${DEPENDENCY_EXTERNAL_COMPONENTS}"`
do
    if [ ! -d "../${component_name}" ]
    then
        error "This script requires the following directories to be copied at project level"
        error "${DEPENDENCY_EXTERNAL_COMPONENTS}"
        exit 1
    fi
done


info "Loading repos/tags for docker images from docker/.env"
. docker/.env
version_label="com.vmware.blockchain.version"
commit_label="com.vmware.blockchain.commit"
daml_sdk_label="com.daml.sdkversion"

verifyDocker

# Default build/Complete product build
if [ -z "${COMPONENTS_TO_BUILD_ON_DEMAND}" ]
then
    info "**** Building all components..."
    node-dependency

    concord
    ui
    waitForProcesses

    fluentd
    ethereum
    waitForProcesses

    helen
    castor
    trc-lib
    waitForProcesses

    client-pool-lib
    waitForProcesses

    participant-lib
    waitForProcesses

    memleak_concord # concord should be built as a pre-req
    persephone
    daml
    trc-test-app
    waitForProcesses

    contract-compiler
    reverse-proxy
    asset-transfer
    supply-chain
    waitForProcesses

    BuildPersephoneGRPCpyBindings
    PerformanceTests
fi

# For additional components are required to be built for specific runs (nightly),
# pass cmd arg (--additionalBuilds <component1,component2>)
if [ ! -z "${ADDITIONAL_BUILDS}" ]
then
    for additional_build in `echo "${ADDITIONAL_BUILDS}" | tr ',' ' '`
    do
        info "**** Building custom component: $additional_build"
        pushd .
        $additional_build &
        popd
        addToProcList "$additional_build" $!
    done
fi

# For building specific components (on demand build, like only persephone, etc)
# pass cmd arg (--additionalBuilds <component1,component2>)
if [ ! -z "${COMPONENTS_TO_BUILD_ON_DEMAND}" ]
then
    for build_on_demand in `echo "${COMPONENTS_TO_BUILD_ON_DEMAND}" | tr ',' ' '`
    do
        info "**** Building custom component: $build_on_demand"
        pushd .
        $build_on_demand
        popd
    done
fi

waitForProcesses
