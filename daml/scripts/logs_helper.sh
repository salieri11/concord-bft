#!/bin/bash

function set_defaults() {
  OPERATION="help"
  CONTAINER_TYPE="execution_engine"
  REMOTE_USER_NAME=root
  REMOTE_PASSWORD=""
}

function read_operation() {
  case $1 in
    help)
    OPERATION="help"
    ;;
    get)
    OPERATION="get"
    ;;
    put)
    OPERATION="put"
    ;;
  esac
}

function read_modifiers() {
  while [[ $# -gt 0 ]]
  do
  KEY=$1

  case $KEY in
      -u)
      REMOTE_USER_NAME=$2
      shift 2
      ;;
      -p)
      REMOTE_PASSWORD=$2
      shift 2
      ;;
      -c)
      CONTAINER_TYPE=$2
      shift 2
      ;;
      *)
      POSITIONAL+=($1) # save it in an array for later
      shift
      ;;
  esac
  done
}

function read_command_line() {
  read_operation $1
  shift
  POSITIONAL=()
  read_modifiers $@
  SOURCE=${POSITIONAL[0]}
  TARGETS=(${POSITIONAL[@]:1})
  REMOTE_TEMP_PATH="/tmp/logback"
}

function print_help() {
  echo "---------- daml containers log helper ----------"
  echo ""
  echo "Use this tool to download and upload log configuration of DAML components."
  echo "Running multiple concurrent instances of this script towards the same host"
  echo "is not supported and can result in unexpected results."
  echo ""
  echo "Commands:"
  echo ""
  echo "'get': copies 'logback.xml' from a container under a remote address to a given relative path."
  echo "If multiple containers match, the first one listed will be chosen."
  echo "example: ./logs_helper.sh get 10.70.30.34 ."
  echo ""
  echo "'put': Distributes a 'logback.xml' stored locally to CONTAINERS under remote addresses."
  echo "example: ./logs_helper.sh put ./logback.xml 10.70.30.34 10.73.232.21"
  echo ""
  echo "Optional modifiers for all commands:"
  echo ""
  echo "'-c': Grep for specific containers running at an address."
  echo "Defaults to 'execution_engine'."
  echo "Example: ./logs_helper.sh get -c execution_engine 10.40.205.57 ."
  echo ""
  echo "'-u': Log in to remote machine as the given user."
  echo "Defaults to 'root'."
  echo "Example: ./logs_helper.sh get -u username 10.40.205.57 ."
  echo ""
  echo "'-p': Log in to remote machine using the given password."
  echo "Example: ./logs_helper.sh get -p password 10.40.205.57 ."
}

function determine_password_usage() {
  if  [[ -z ${REMOTE_PASSWORD} ]]; then
    PASS_OPT=""
  else
    PASS_OPT="sshpass -p ${REMOTE_PASSWORD}"
  fi
}

function get_remote_containers() {
  CONTAINERS=($(${PASS_OPT} \
    ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@$1 \
      "docker ps --filter name=${CONTAINER_TYPE} --format {{.Names}}"))
  
  if [[ ${#CONTAINERS[@]} -lt 1 ]]; then 
    echo "No remote container matching the name '${CONTAINER_TYPE}'!"
    exit 1
  fi
}

function extract_logback_config_file() {
  $PASS_OPT ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@$1 bash -s <<EOF
    docker exec $2 /bin/bash -c "cat entrypoint.sh" | \
      grep "LOGBACK_CONFIG_FILE=" | \
      cut -d '"' -f 2
    exit
EOF
}

function calculate_container_path() {
  LOGBACK_CONFIG_FILE=$(extract_logback_config_file $1 $2)
  if [[ -z ${LOGBACK_CONFIG_FILE} ]]; then
    echo "Couldn't find LOGBACK_CONFIG_FILE definition in the entrypoint of '$2'!"
    exit 1
  fi
  CONTAINER_PATH="$2:${LOGBACK_CONFIG_FILE}"
}

function get_logback() {
  LOGBACK_PATH=${TARGETS[0]}
  REMOTE_IP=${SOURCE}

  echo "Getting 'logback.xml' from '${REMOTE_IP}' to '${LOGBACK_PATH}'"
  get_remote_containers ${REMOTE_IP}

  if [[ ${#CONTAINERS[@]} -gt 1 ]]; then 
    echo "Multiple containers match criteria: (${CONTAINERS[@]})!"
    exit 1
  fi

  calculate_container_path ${REMOTE_IP} ${CONTAINERS[0]}

  $PASS_OPT ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@${REMOTE_IP} bash -s <<EOF
    mkdir -p ${REMOTE_TEMP_PATH}
    rm -f ${REMOTE_TEMP_PATH}/logback.xml 2>/dev/null
    docker cp -a ${CONTAINER_PATH} ${REMOTE_TEMP_PATH}
    exit
EOF

  mkdir -p ./${LOGBACK_PATH}
  ${PASS_OPT} scp -r \
    ${REMOTE_USER_NAME}@${REMOTE_IP}:${REMOTE_TEMP_PATH}/logback.xml \
    ./${LOGBACK_PATH}
  echo "Saved to '${LOGBACK_PATH}/logback.xml'"
}

function put_logback() {
  LOGBACK_PATH=${SOURCE}

  if [ ! -f "${LOGBACK_PATH}" ]; then
    echo "'${LOGBACK_PATH}' does not exist!"
    exit 1
  fi

  if [[ ${#TARGETS[@]} -lt 1 ]]; then
    echo "No remote addresses specified!"
    exit 1
  fi

  DATE=$(date -u '+%Y%m%d_%H%M%SZ')

  for REMOTE_IP in "${TARGETS[@]}"; do

    echo "Uploading '${LOGBACK_PATH}' to '${REMOTE_IP}', backups are stored remotely at ${REMOTE_TEMP_PATH}"

    ${PASS_OPT} ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@${REMOTE_IP} \
        "mkdir -p ${REMOTE_TEMP_PATH}"
    ${PASS_OPT} scp -o StrictHostKeyChecking=no -r \
      ${LOGBACK_PATH} ${REMOTE_USER_NAME}@${REMOTE_IP}:${REMOTE_TEMP_PATH}/logback.xml
    ${PASS_OPT} ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@${REMOTE_IP} \
        "chmod 666 ${REMOTE_TEMP_PATH}/logback.xml"

    get_remote_containers ${REMOTE_IP}

    for CONTAINER in ${CONTAINERS[@]}; do
      calculate_container_path ${REMOTE_IP} ${CONTAINER}
      echo "Pushing '${REMOTE_TEMP_PATH}/logback.xml' to '${CONTAINER_PATH}'"

      ${PASS_OPT} ssh -o StrictHostKeyChecking=no ${REMOTE_USER_NAME}@${REMOTE_IP} \
        bash -s <<EOF
          docker cp ${CONTAINER_PATH} ${REMOTE_TEMP_PATH}/logback_${CONTAINER}_${DATE}.xml
          docker cp ${REMOTE_TEMP_PATH}/logback.xml ${CONTAINER_PATH}
          exit
EOF
    done
  done
}

set_defaults
read_command_line $@

if  [[ ${OPERATION} = "help" ]]; then
  print_help
elif  [[ ${OPERATION} = "get" ]]; then
  get_logback
elif  [[ ${OPERATION} = "put" ]]; then
  put_logback
fi
