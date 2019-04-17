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
  echo A problem or interrupt occurred. Killing processes...

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
    echo Error: Docker was not found on this system.
    exit 1
  else
    echo Docker: "${DOCKER}"
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

    echo "-------- Status --------"

    for i in "${!BUILD_PROCS[@]}"
    do
      STATUS_STRING=""

      if isRunning ${BUILD_PROCS[$i]}
      then
        DONE=false
        STATUS_STRING="waiting"
	echo "${i}": "${STATUS_STRING}"
      else
        STATUS_STRING="done"
        echo "${i}": "${STATUS_STRING}"
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
    echo ERROR: Failure executing "${NAME}".  Exit code: ${EXIT_CODE}
    killAllProcs
    exit 1
  fi
}

# Accepting a human-friendly name and process ID, adds
# them to the BUILD_PROCS array of name=procId.
addToProcList(){
  echo Adding build process: "${1}"=${2}
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
    echo Waiting for maven build of helen/ethrpc/communication...
    sleep 10
  done

  dieOnFailure "Maven" ${MVN_BUILD}
}

# Note that the SimpleStateTransferTest relies on a local Concord build
# for a test file (blockchain/concord/build/tools/conc_rocksdb_adp).
# If we can make that test work entirely in a container, this can be removed.
startNativeConcordBuild(){
    pushd .
    cd concord
    #echo "sed -i \"s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g\" test/resources/concord1.config"
    #sed -i "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord1.config
    #sed -i "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord2.config
    #sed -i "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord3.config
    #sed -i "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord4.config

    git submodule init
    git submodule update --recursive
    mkdir -p build
    cd build
    cmake .. > concord_native_cmake.log 2>&1
    make > concord_native_make.log 2>&1 &
    addToProcList "Concord_native" $!
    popd
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
    echo "Waiting for $NAME..."
    sleep 10
  done

  dieOnFailure "${NAME}" ${NODE_BUILD_PID}
  popd

}

docker_build() {
    if [ "$#" -lt "4" ]
    then
        echo "Missing Required Parameters for docker build method"
        echo "Usage: docker_build <directory to docker build> <docker build file> <docker build name> <docker build tag>"
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
        memleak_util_cmd="valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=/tmp/valgrind_concord1.log"
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
        echo "Missing Required Parameters for docker pull method"
        echo "Usage: docker_pull <docker image:tag> <Task Name>"
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
    mvn clean install assembly:single > performance__test_mvn_build.log 2>&1
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
        echo "This script requires the following directories to be copied at project level"
        echo "${DEPENDENCY_EXTERNAL_COMPONENTS}" | tr ' ' '\n'
        exit 1
    fi
done

echo Loading repos/tags for docker images from docker/.env
. docker/.env
version_label="com.vmware.blockchain.version"
commit_label="com.vmware.blockchain.commit"

verifyDocker

echo "Installing node package dependencies..."
install_node_dependency "node dependency for UI" "ui"
install_node_dependency "node dependency for contract-compiler" "contract-compiler"

echo "Building..."
docker_build . concord/Dockerfile ${concord_repo} ${concord_tag}

docker_build . concord/Dockerfile ${concord_repo} ${concord_tag} --memoryLeakDockerBuild

# RV: March 21, 2019: This is only needed for the state transfer tests.  Removing.
# startNativeConcordBuild

docker_build ui ui/Dockerfile ${ui_repo} ${ui_tag}

docker_build docker/fluentd docker/fluentd/Dockerfile ${fluentd_repo} ${fluentd_tag}

# Includes helen, ethrpc, and communication.
buildMavenTargets

docker_build ethrpc ethrpc/packaging.Dockerfile ${ethrpc_repo} ${ethrpc_tag}

docker_build helen helen/packaging.Dockerfile ${helen_repo} ${helen_tag}

docker_build . persephone/metadata-service/Dockerfile ${persephone_metadata_repo} ${persephone_tag}
docker_build . persephone/provision-service/Dockerfile ${persephone_provisioning_repo} ${persephone_tag}
# docker_build . persephone/fleet/Dockerfile ${persephone_fleet_repo} ${persephone_tag}

docker_pull cockroachdb/cockroach:v2.0.2 Cockroach_DB

docker_pull athena-docker-local.artifactory.eng.vmware.com/reverse-proxy:0.1.2 "Reverse_proxy"

docker_build asset-transfer asset-transfer/Dockerfile ${asset_transfer_repo} ${asset_transfer_tag}

docker_build agent agent/packaging.Dockerfile ${agent_repo} ${agent_tag}

docker_build contract-compiler contract-compiler/Dockerfile ${contract_compiler_repo} ${contract_compiler_tag}

if [ ! -z "${ADDITIONAL_BUILDS}" ]
then
    for additional_build in `echo "${ADDITIONAL_BUILDS}" | tr ',' ' '`
    do
        echo "Building custom component: $additional_build"
        pushd .
        $additional_build &
        popd
        addToProcList "$additional_build" $!
    done
fi

waitForProcesses
