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

declare -A BUILD_PROCS

echo Loading repos/tags for docker images from docker/.env
. docker/.env

verifyDocker

echo "Building..."
docker build . --file concord/Dockerfile -t ${concord_repo}:${concord_tag} > concord_build.log 2>&1 &
addToProcList "Concord" $!

docker build ui --file ui/Dockerfile -t ${ui_repo}:${ui_tag} > ui_build.log 2>&1 &
addToProcList "UI" $!

docker build docker/fluentd --file docker/fluentd/Dockerfile -t ${fluentd_repo}:${fluentd_tag} > fluentd_build.log 2>&1 &
addToProcList "Fluentd" $!

# Includes helen, ethrpc, and communication.
buildMavenTargets

docker build ethrpc -f ethrpc/packaging.Dockerfile -t ${ethrpc_repo}:${ethrpc_tag} > ethrpc_build_docker.log 2>&1 &
addToProcList "Ethrpc docker image" $!

docker build helen -f helen/packaging.Dockerfile -t ${helen_repo}:${helen_tag} > helen_build.log 2>&1 &
addToProcList "Helen docker image" $!

docker pull cockroachdb/cockroach:v2.0.2 &
addToProcList "Cockroach DB" $!

docker pull athena-docker-local.artifactory.eng.vmware.com/reverse-proxy:0.1.2 &
addToProcList "Reverse proxy" $!

waitForProcesses
