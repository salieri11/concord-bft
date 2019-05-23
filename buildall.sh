#!/bin/bash

################################################################################
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

trap killAllProcs INT

# Kill all processes.
killAllProcs(){
  error "A problem or interrupt occurred. Killing processes..."

  for i in "${!BUILD_PROCS[@]}"
  do
    if isRunning ${BUILD_PROCS[$i]}
    then
      kill ${BUILD_PROCS[$i]}
    fi
  done
}

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

# Accept a process ID and return 0/success if running,
# nonzero/failure if not running.
isRunning(){
  ps -p ${1} > /dev/null
  return $?
}

# Waits for all processes in the BUILD_PROCS associative array to end.
waitForProcesses(){
  DONE=false

  while [ "$DONE" = false ]
  do
    DONE=true

    info "-------- Status --------"
    printMemory

    for i in "${!BUILD_PROCS[@]}"
    do
      STATUS_STRING=""

      if isRunning ${BUILD_PROCS[$i]}
      then
        DONE=false
        STATUS_STRING="waiting"
	info "${i}: ${STATUS_STRING}"
      else
        STATUS_STRING="done"
        info "${i}: ${STATUS_STRING}"
        dieOnFailure "${i}" ${BUILD_PROCS[$i]}
      fi
    done

    sleep 5
  done
}

# Checks the exit code of the given process id, and exits
# this entire script if it indicates failure.
dieOnFailure(){
  NAME="${1}"
  PID=${2}
  wait ${PID}
  EXIT_CODE=$?

  if [ ${EXIT_CODE} -ne 0 ]
  then
    error "Failure executing ${NAME}.  Exit code: ${EXIT_CODE}"
    killAllProcs
    exit 1
  fi
}

# Accepting a human-friendly name and process ID, adds
# them to the BUILD_PROCS array of name=procId.
addToProcList(){
  info "Adding build process: ${1}=${2}"
  BUILD_PROCS["${1}"]=${2}
}

# Builds the maven targets.
buildMavenTargets(){
  docker volume create --name mvn-repo
  docker run \
    --rm --name mvn-build \
    --user $(id -u):$(id -g) \
    -v maven-repo:/root/.m2 \
    -v "$(pwd)":/workspace \
    -w /workspace \
    athena-docker-local.artifactory.eng.vmware.com/build-images/maven-builder:v1 \
    mvn clean install > mvn_build.log 2>&1 &
  MVN_BUILD=$!

  addToProcList "Maven" $!

  while isRunning ${MVN_BUILD}
  do
    info "Waiting for maven build of helen/ethrpc/communication..."
    sleep 10
  done

  dieOnFailure "Maven" ${MVN_BUILD}
}


install_node_dependency() {
  NAME="${1}"
  COMPONENT_DIR="${2}"

  pushd .
  cd "$COMPONENT_DIR"
  npm install > "node_install_${COMPONENT_DIR}.log" 2>&1 &
  NODE_BUILD_PID=$!
  addToProcList "${NAME}" $!

  while isRunning ${NODE_BUILD_PID}
  do
    info "Waiting for $NAME..."
    sleep 10
  done

  dieOnFailure "${NAME}" ${NODE_BUILD_PID}
  popd

}

docker_build() {
    if [ "$#" -lt "4" ]
    then
        error "Missing Required Parameters for docker build method"
        error "Usage: docker_build <directory to docker build> <docker build file> <docker build name> <docker build tag>"
        killAllProcs
        exit 1
    fi
    DOCKER_BUILD_DIR="$1"
    shift
    DOCKER_BUILD_FILE="$1"
    shift
    DOCKER_REPO_NAME="$1"
    shift
    DOCKER_REPO_TAG="$1"
    shift

    MEMORY_LEAK_DOCKER_BUILD=""
    while [ "$1" != "" ] ; do
       case $1 in
          "--memoryLeakDockerBuild")
             MEMORY_LEAK_DOCKER_BUILD="$1"
             ;;
       esac
       shift
    done

    if [ ! -z "${MEMORY_LEAK_DOCKER_BUILD}" ]
    then
        memleak_util="valgrind"
        memleak_util_cmd="valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=/tmp/valgrind_concord1.log --suppressions=/concord/concord.supp"
        docker build "${DOCKER_BUILD_DIR}" -f "${DOCKER_BUILD_FILE}" -t "${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG}"_memleak --build-arg "memleak_util=${memleak_util}" --build-arg "memleak_util_cmd=${memleak_util_cmd}" > concord_memleak_build.log 2>&1 &
        addToProcList "Concord_for_memleak_image" $!
    else
        docker build "${DOCKER_BUILD_DIR}" -f "${DOCKER_BUILD_FILE}" -t "${DOCKER_REPO_NAME}:${DOCKER_REPO_TAG}" --label ${version_label}=${DOCKER_REPO_TAG} --label ${commit_label}=${commit_hash} > `basename "${DOCKER_REPO_NAME}"_build.log` 2>&1 &
        addToProcList `basename "${DOCKER_REPO_NAME}_image"` $!
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
    DOCKER_IMAGE_WITH_TAG="$1"
    DOCKER_PULL_TASK_NAME="$2"

    docker pull "${DOCKER_IMAGE_WITH_TAG}" &
    addToProcList "${DOCKER_PULL_TASK_NAME}" $!
}

PerformanceTests() {
    cd performance
    mvn clean install assembly:single > performance_test_mvn_build.log 2>&1
}

BuildPersephoneGRPCpyBindings() {
    pushd .
    ./hermes/util/generate_gRPC_bindings_for_py3.sh --protobufSrc persephone/api/src/protobuf > persephone_generate_gRPC_py_bindings.log 2>&1 &
    addToProcList "Persephone gRPC Python Buildings" $!
    popd
}

info() {
    echo `date`: INFO: "${1}"
}

error() {
    echo `date`: ERROR: "${1}"
}

printMemory() {
    uname | grep "Linux"
    LINUX=$?

    if [ $LINUX -eq 0 ]
    then
        free -h
    fi
}

declare -A BUILD_PROCS

while [ "$1" != "" ] ; do
   case $1 in
      "--additionalBuilds")
         shift
         ADDITIONAL_BUILDS="$1"
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

verifyDocker

info "Installing node package dependencies..."
install_node_dependency "node dependency for UI" "ui"
install_node_dependency "node dependency for contract-compiler" "contract-compiler"

info "Building..."
docker_build . concord/Dockerfile ${concord_repo} ${concord_tag}

docker_build . concord/Dockerfile ${concord_repo} ${concord_tag} --memoryLeakDockerBuild

docker_build ui ui/Dockerfile ${ui_repo} ${ui_tag}

docker_build docker/fluentd docker/fluentd/Dockerfile ${fluentd_repo} ${fluentd_tag}

# Includes helen, ethrpc, and communication.
buildMavenTargets

docker_build ethrpc ethrpc/packaging.Dockerfile ${ethrpc_repo} ${ethrpc_tag}

docker_build helen helen/packaging.Dockerfile ${helen_repo} ${helen_tag}

docker_build persephone persephone/metadata-service/Dockerfile ${persephone_metadata_repo} ${persephone_tag}
docker_build . persephone/provision-service/Dockerfile ${persephone_provisioning_repo} ${persephone_tag}
# docker_build persephone persephone/fleet/Dockerfile ${persephone_fleet_repo} ${persephone_tag}

docker_pull cockroachdb/cockroach:v2.0.2 Cockroach_DB

docker_pull athena-docker-local.artifactory.eng.vmware.com/reverse-proxy:0.1.2 "Reverse_proxy"

docker_build asset-transfer asset-transfer/Dockerfile ${asset_transfer_repo} ${asset_transfer_tag}

docker_build agent agent/packaging.Dockerfile ${agent_repo} ${agent_tag}

docker_build contract-compiler contract-compiler/Dockerfile ${contract_compiler_repo} ${contract_compiler_tag}

BuildPersephoneGRPCpyBindings

if [ ! -z "${ADDITIONAL_BUILDS}" ]
then
    for additional_build in `echo "${ADDITIONAL_BUILDS}" | tr ',' ' '`
    do
        info "Building custom component: $additional_build"
        pushd .
        $additional_build &
        popd
        addToProcList "$additional_build" $!
    done
fi

waitForProcesses
