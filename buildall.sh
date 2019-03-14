#!/bin/bash

################################################################################
# High level script to build the blockchain components.
# To add a component:
#   Go to the commands at the end of the script.
#   Add a line to start building the component in a background process:
#     <command to build foo> &
#   Add a line to store that background process in the process list:
#     addToProcList "Foo" $!
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
    -v maven-repo:/root/.m2 \
    -v "$(pwd)":/workspace \
    -w /workspace maven:3.6.0-jdk-11 \
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

echo Loading repos/tags for docker images from docker/.env
. docker/.env
version_label="com.vmware.blockchain.version"
commit_label="com.vmware.blockchain.commit"

verifyDocker

echo "Building..."
docker build . --file concord/Dockerfile -t ${concord_repo}:${concord_tag} --label ${version_label}=${concord_tag} --label ${commit_label}=${commit_hash} > concord_build.log 2>&1 &
addToProcList "Concord" $!

memleak_util="valgrind"
memleak_util_cmd="valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=/tmp/valgrind_concord1.log"
docker build . --file concord/Dockerfile -t "${concord_repo}:${concord_tag}"_memleak --build-arg "memleak_util=${memleak_util}" --build-arg "memleak_util_cmd=${memleak_util_cmd}" > concord_memleak_build.log 2>&1 &
addToProcList "Concord_for_memleak" $!

startNativeConcordBuild

docker build ui --file ui/Dockerfile -t ${ui_repo}:${ui_tag} --label ${version_label}=${ui_tag} --label ${commit_label}=${commit_hash} > ui_build.log 2>&1 &
addToProcList "UI" $!

docker build docker/fluentd --file docker/fluentd/Dockerfile -t ${fluentd_repo}:${fluentd_tag} --label ${version_label}=${fluentd_tag} --label ${commit_label}=${commit_hash} > fluentd_build.log 2>&1 &
addToProcList "Fluentd" $!

# Includes helen, ethrpc, and communication.
buildMavenTargets

docker build ethrpc -f ethrpc/packaging.Dockerfile -t ${ethrpc_repo}:${ethrpc_tag} --label ${version_label}=${ethrpc_tag} --label ${commit_label}=${commit_hash} > ethrpc_build_docker.log 2>&1 &
addToProcList "Ethrpc_docker_image" $!

docker build helen -f helen/packaging.Dockerfile -t ${helen_repo}:${helen_tag} --label ${version_label}=${helen_tag} --label ${commit_label}=${commit_hash} > helen_build.log 2>&1 &
addToProcList "Helen_docker_image" $!

docker build . -f persephone/metadata-service/Dockerfile -t ${persephone_repo}:${persephone_tag}  --label ${version_label}=${persephone_tag} --label ${commit_label}=${commit_hash} > persephone_build.log 2>&1 &
addToProcList "Fleet Management docker image" $!

docker pull cockroachdb/cockroach:v2.0.2 &
addToProcList "Cockroach_DB" $!

docker pull athena-docker-local.artifactory.eng.vmware.com/reverse-proxy:0.1.2 &
addToProcList "Reverse_proxy" $!

docker build asset-transfer -f asset-transfer/Dockerfile -t "${asset_transfer_repo}:${asset_transfer_tag}" --label ${version_label}=${asset_transfer_tag} --label ${commit_label}=${commit_hash} > asset_transfer_build.log 2>&1 &
addToProcList "Asset_Transfer_sample_image" $!

docker build agent -f agent/packaging.Dockerfile -t ${agent_repo}:${agent_tag} --label ${version_label}=${agent_tag} --label ${commit_label}=${commit_hash} > agent_build.log 2>&1 &
addToProcList "Agent_docker_image" $!

if [ ! -z "${ADDITIONAL_BUILDS}" ]
then
    for additional_build in `echo "${ADDITIONAL_BUILDS}" | tr ',' ' '`
    do
        echo "Building custom component: $additional_build"
        $additional_build &
        addToProcList "$additional_build" $!
    done
fi

waitForProcesses
