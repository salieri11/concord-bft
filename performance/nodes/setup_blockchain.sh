#!/bin/sh

BLOCKCHAIN_REPO_NAME="blockchain"
BLOCKCHAIN_REPO="../../../${BLOCKCHAIN_REPO_NAME}"
WORKDIR=workdir
DOCKER_LOG_DIR=${WORKDIR}/logs/docker_logs
ALL_NODES_IPS=""

while [ "$1" != "" ] ; do
   case $1 in
      "--concordIPs")
         shift
         CONCORD_IPS=$1
         ;;
      "--helenIP")
         shift
         HELEN_NODE=$1
         ;;
      "--username")
         shift
         NODE_USERNAME=$1
         ;;
      "--password")
         shift
         NODE_PASSWORD=$1
         ;;
      "--useLocalDockerImages")
         USE_LOCAL_DOCKER_IMAGES="--useLocalDockerImages"
         ;;
   esac
   shift
done

usage() {
    echo "Usage: $0 --concordIPs <Concord-IP1>,<Concord-IP2>,<Concord-IP3>,<Concord-IP4>"
    echo "          --helenIP <Helen IP>"
    echo "          --username <username on concord/helen VMs>"
    echo "          --password <password for concord/helen username>"
    echo "          [Optional - To use local docker images: --useLocalDockerImages]"
    exit 1
}

setup_docker() {
    echo "\nSetting up docker login on all nodes"
    echo "Enter docker login username: \c"
    read DOCKER_USERNAME
    echo "Enter docker login password: \c"
    stty -echo
    read DOCKER_PASSWORD
    stty echo
    echo ""

    ALL_NODES_IPS="${CONCORD_IPS},""${HELEN_NODE}"
    for CONCORD_NODE in `echo $ALL_NODES_IPS | tr ',' '\n'`
    do
        echo "\nNode: $CONCORD_NODE..."
        echo "${NODE_PASSWORD}" | ssh ${NODE_USERNAME}@${CONCORD_NODE} sudo -S usermod -a -G docker ${NODE_USERNAME} > /dev/null 2>&1
        echo "${DOCKER_PASSWORD}" | ssh "${NODE_USERNAME}@${CONCORD_NODE}" "docker login --username ${DOCKER_USERNAME} --password-stdin"
    done
}

pull_docker_images() {
    NODE_USERNAME="$1"
    NODE="$2"
    DOCKER_IMAGES="$3"
    IMAGE_TYPE="$4"

    echo "docker pull ${IMAGE_TYPE} images (could take a min or 2)..."
    for DOCKER_IMAGE in `echo "${DOCKER_IMAGES}"`
    do
        RUNNING_CONTAINER=`ssh "${NODE_USERNAME}@${NODE}" docker ps -a | grep "${DOCKER_IMAGE}" | awk '{print $1}'`
        if [ ! -z "${RUNNING_CONTAINER}" ]
        then
            echo "Stop & kill the running container ${DOCKER_IMAGE}..."
            ssh "${NODE_USERNAME}@${NODE}" "docker stop ${RUNNING_CONTAINER} && docker rm ${RUNNING_CONTAINER}" > /dev/null 2>&1
        fi
    done
    echo ""

    for DOCKER_IMAGE in `echo "${DOCKER_IMAGES}"`
    do
        echo "docker pull ${DOCKER_IMAGE}..."
        if [ "${USE_LOCAL_DOCKER_IMAGES}" = "" ]
        then
            ssh "${NODE_USERNAME}@${NODE}" docker rmi ${DOCKER_IMAGE} > /dev/null 2>&1
        fi
        DOCKER_LOG_FILE=`echo ${DOCKER_IMAGE} | tr '/' '_'`
        DOCKER_LOG_FILE="docker_pull_${DOCKER_LOG_FILE}_${NODE}.log"
        echo "**** docker pull ****" > ${DOCKER_LOG_DIR}/${DOCKER_LOG_FILE} # /dev/null 2>&1
        ssh "${NODE_USERNAME}@${NODE}" docker pull ${DOCKER_IMAGE}:latest >> ${DOCKER_LOG_DIR}/${DOCKER_LOG_FILE} # /dev/null 2>&1
        echo "\n**** docker inspect ****" >> ${DOCKER_LOG_DIR}/${DOCKER_LOG_FILE} # /dev/null 2>&1
        ssh "${NODE_USERNAME}@${NODE}" docker inspect ${DOCKER_IMAGE}:latest >> ${DOCKER_LOG_DIR}/${DOCKER_LOG_FILE} # /dev/null 2>&1
    done
    ssh "${NODE_USERNAME}@${NODE}" docker images
}

setup_concord_nodes() {
    echo "Copy config-public from blockchain repo..."
    cp -r ${BLOCKCHAIN_REPO}/docker/config-public ${WORKDIR}/

    echo "Updating find-docker-instances.sh with concord IPs..."
    HOSTNAMES_STRING_TO_REPLACE='HOSTNAMES=("concord1" "concord2" "concord3" "concord4")'
    HOSTNAMES_TO_REPLACE_WITH=`echo ${CONCORD_IPS} | tr ',' ' '`
    HOSTNAMES_TO_REPLACE_WITH="HOSTNAMES=(${HOSTNAMES_TO_REPLACE_WITH})"
    sed -i.bkp "s/${HOSTNAMES_STRING_TO_REPLACE}/${HOSTNAMES_TO_REPLACE_WITH}/g" ${WORKDIR}/config-public/find-docker-instances.sh

    LOOPBACK_ADDRESS="127.0.0.1"
    for NODE_COUNT in $(seq 1 $NO_OF_IPS)
    do
        echo "Updating docker/config-concord${NODE_COUNT}/concord.config..."
        CONCORD_CONFIG_ORIG_FILE="${BLOCKCHAIN_REPO}/docker/config-concord${NODE_COUNT}/concord.config.orig"
        if [ ! -f "${CONCORD_CONFIG_ORIG_FILE}" ]
        then
            cp ${BLOCKCHAIN_REPO}/docker/config-concord${NODE_COUNT}/concord.config ${BLOCKCHAIN_REPO}/docker/config-concord${NODE_COUNT}/concord.config.orig
        fi
        i=1
        for CONCORD_NODE in `echo $CONCORD_IPS | tr ',' '\n'`
        do
            if [ "${NODE_COUNT}" -eq "${i}" ]
            then
                sed -i.bkp "s/concord${i}/${LOOPBACK_ADDRESS}/g" ${BLOCKCHAIN_REPO}/docker/config-concord${NODE_COUNT}/concord.config
            else
                sed -i.bkp "s/concord${i}/${CONCORD_NODE}/g" ${BLOCKCHAIN_REPO}/docker/config-concord${NODE_COUNT}/concord.config
            fi
            i=`expr $i + 1`
        done
    done

    i=1
    for CONCORD_NODE in `echo $CONCORD_IPS | tr ',' '\n'`
    do
        echo "\n###################### Concord Node $i: $CONCORD_NODE ######################"
        echo ""
        DOCKER_IMAGES="vmwblockchain/concord-core vmwblockchain/ethrpc"
        pull_docker_images "${NODE_USERNAME}" "${CONCORD_NODE}" "${DOCKER_IMAGES}" "concord"

        echo "Remove any existing configuration..."
        echo "${NODE_PASSWORD}" | ssh ${NODE_USERNAME}@${CONCORD_NODE} sudo -S rm -rf /home/concord/concord > /dev/null 2>&1
        ssh ${NODE_USERNAME}@${CONCORD_NODE} 'mkdir -p' "/home/${NODE_USERNAME}/concord"

        scp -r ${WORKDIR}/config-public/ ${NODE_USERNAME}@${CONCORD_NODE}:/home/${NODE_USERNAME}/concord
        scp -r ${BLOCKCHAIN_REPO}/docker/config-concord${i}/ ${NODE_USERNAME}@${CONCORD_NODE}:/home/${NODE_USERNAME}/concord
        scp -r ${BLOCKCHAIN_REPO}/docker/config-ethrpc${i}/ ${NODE_USERNAME}@${CONCORD_NODE}:/home/${NODE_USERNAME}/concord
        scp -r ${BLOCKCHAIN_REPO}/docker/tls_certs/ ${NODE_USERNAME}@${CONCORD_NODE}:/home/${NODE_USERNAME}/concord
        scp "docker_compose_files/docker-compose_concord_${i}.yml" ${NODE_USERNAME}@${CONCORD_NODE}:/home/${NODE_USERNAME}/concord/docker-compose.yml

        ssh ${NODE_USERNAME}@${CONCORD_NODE} "cd /home/${NODE_USERNAME}/concord && docker-compose up" > ${DOCKER_LOG_DIR}/docker_compose_concord_${CONCORD_NODE}.log &
        sleep 10
        echo "Node $i setup Completed!"
        i=`expr $i + 1`
    done
    echo "\ndocker-compose up on all concord nodes... done!\n"
}

setup_helen() {
    echo "###################### Helen Node: $HELEN_NODE ######################"
    echo ""
    DOCKER_IMAGES="vmwblockchain/concord-ui cockroachdb/cockroach"
    pull_docker_images "${NODE_USERNAME}" "${HELEN_NODE}" "${DOCKER_IMAGES}" "helen"

    echo "Remove any existing configuration..."
    echo "${NODE_PASSWORD}" | ssh ${NODE_USERNAME}@${HELEN_NODE} sudo -S rm -rf /home/${NODE_USERNAME}/helen > /dev/null 2>&1
    ssh ${NODE_USERNAME}@${HELEN_NODE} "mkdir -p /home/${NODE_USERNAME}/helen"

    echo "Updating ConcordAuthorities with concord IPs..."
    cp -r ${BLOCKCHAIN_REPO}/helen/src/main/resources/application.properties ${WORKDIR}/
    CONCORD_AUTHORITIES_STRING_TO_REPLACE='ConcordAuthorities=localhost:5458,localhost:5459,localhost:5460,localhost:5461'
    CONCORD_AUTHORITIES=""
    i=1
    for CONCORD_NODE in `echo $CONCORD_IPS | tr ',' '\n'`
    do
        CONCORD_AUTHORITIES="${CONCORD_AUTHORITIES}${CONCORD_NODE}:5458"
        if [ "$i" -lt "$NO_OF_IPS" ]
        then
            CONCORD_AUTHORITIES="${CONCORD_AUTHORITIES},"
        fi
        i=`expr $i + 1`
    done
    CONCORD_AUTHORITIES_TO_REPLACE_WITH="ConcordAuthorities=${CONCORD_AUTHORITIES}"
    sed -i.bkp "s/${CONCORD_AUTHORITIES_STRING_TO_REPLACE}/${CONCORD_AUTHORITIES_TO_REPLACE_WITH}/g" ${WORKDIR}/application.properties

    echo "Updating ConcordRpcUrls with concord IPs..."
    CONCORD_RPC_URLS_STRING_TO_REPLACE='ConcordRpcUrls=replica0=https://127.0.0.1:8545/,replica1=https://127.0.0.1:8546/,replica2=https://127.0.0.1:8547/,replica3=https://127.0.0.1:8548/'
    CONCORD_RPC_URLS=""
    i=0
    for CONCORD_NODE in `echo $CONCORD_IPS | tr ',' '\n'`
    do
        CONCORD_RPC_URLS="${CONCORD_RPC_URLS}replica${i}=https://${CONCORD_NODE}:8545/"
        if [ `expr $i + 1` -lt "$NO_OF_IPS" ]
        then
            CONCORD_RPC_URLS="${CONCORD_RPC_URLS},"
        fi
        i=`expr $i + 1`
    done
    CONCORD_RPC_URLS_TO_REPLACE_WITH="ConcordRpcUrls=${CONCORD_RPC_URLS}"
    sed -i.bkp "s?${CONCORD_RPC_URLS_STRING_TO_REPLACE}?${CONCORD_RPC_URLS_TO_REPLACE_WITH}?g" ${WORKDIR}/application.properties
    scp ${WORKDIR}/application.properties ${NODE_USERNAME}@${HELEN_NODE}:/home/${NODE_USERNAME}/helen/

    echo "Updating application-test.properties..."
    echo "ConcordAuthorities=${CONCORD_AUTHORITIES}" > ${WORKDIR}/application-test.properties
    echo "DB_IP=db-server" >> ${WORKDIR}/application-test.properties
    scp ${WORKDIR}/application-test.properties ${NODE_USERNAME}@${HELEN_NODE}:/home/${NODE_USERNAME}/helen/

    scp -r ${BLOCKCHAIN_REPO}/docker/config-helen/ ${NODE_USERNAME}@${HELEN_NODE}:/home/${NODE_USERNAME}/helen/
    scp -r ${BLOCKCHAIN_REPO}/helen/src/main/resources/database/ ${NODE_USERNAME}@${HELEN_NODE}:/home/${NODE_USERNAME}/helen/
    scp "docker_compose_files/docker-compose-helen.yml" ${NODE_USERNAME}@${HELEN_NODE}:/home/${NODE_USERNAME}/helen/docker-compose.yml

    ssh ${NODE_USERNAME}@${HELEN_NODE} "cd /home/${NODE_USERNAME}/helen && docker-compose up" > ${DOCKER_LOG_DIR}/docker_compose_helen_${HELEN_NODE}.log &
    sleep 15
    echo "\ndocker-compose up on helen node... done!"
}

if [ -z "${CONCORD_IPS}" -o -z "${HELEN_NODE}" -o -z "${NODE_USERNAME}" -o -z "${NODE_PASSWORD}" ]
then
    usage
fi

NO_OF_IPS=`echo "$CONCORD_IPS" | awk -F',' '{ print NF }'`
if [ "$NO_OF_IPS" != "4" ]
then
    echo "\nCurrently, this script supports only 4 concord nodes"
    echo "Please reach out to blockchain team for any enhancement request"
    exit 1
fi

if [ ! -z "$USE_LOCAL_DOCKER_IMAGES" ]
then
    echo "\n**** THIS RUN WILL USE LOCAL DOCKER IMAGES, if AVAILABLE ON THE VM ****"
fi

if [ -d ${WORKDIR} ]
then
    rm -rf ${WORKDIR}
fi
mkdir ${WORKDIR}
mkdir -p ${DOCKER_LOG_DIR}

if [ ! -d "${BLOCKCHAIN_REPO}" ]
then
    echo "\nThis script is part of ${BLOCKCHAIN_REPO_NAME} repo"
    echo "Please clone blockchain repo and follow the instructions in performance/README"
    exit 1
fi

setup_docker
setup_concord_nodes
setup_helen

echo "\nPlease find logs at: ${DOCKER_LOG_DIR}\n"
exit 0
