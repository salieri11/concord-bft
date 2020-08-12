#!/bin/bash

deploy_images () {
  export VDAML_IMAGES=(daml-execution-engine daml-ledger-api daml-index-db concord-core fluentd)
  for i in ${VDAML_IMAGES[@]}; do 
    GCR_NAME=eu.gcr.io/da-dev-concord2/${i}:latest
    DOCKER_NAME=${i}:latest
    docker tag $DOCKER_NAME $GCR_NAME
    docker push $GCR_NAME
    gcloud compute ssh "${GCP_TARGET}" -- "docker pull $GCR_NAME && docker tag $GCR_NAME $DOCKER_NAME"
  done
}

deploy_scripts () {
  gcloud compute ssh "${GCP_TARGET}" -- "
    mkdir -p ~/vmwathena_blockchain/daml/gcp &&
    mkdir -p ~/vmwathena_blockchain/docker/config-public &&
    mkdir -p ~/vmwathena_blockchain/docker/tls_certs"
  gcloud compute scp --recurse daml/gcp/* ${GCP_TARGET}:~/vmwathena_blockchain/daml/gcp
  gcloud compute scp --recurse docker/*.sh ${GCP_TARGET}:~/vmwathena_blockchain/docker
  gcloud compute scp --recurse docker/*.yml ${GCP_TARGET}:~/vmwathena_blockchain/docker
  gcloud compute scp --recurse docker/.env ${GCP_TARGET}:~/vmwathena_blockchain/docker
  gcloud compute scp --recurse docker/config-public/* ${GCP_TARGET}:~/vmwathena_blockchain/docker/config-public
  gcloud compute scp --recurse docker/tls_certs/* ${GCP_TARGET}:~/vmwathena_blockchain/docker/tls_certs
  gcloud compute scp --recurse docker/config-concord* ${GCP_TARGET}:~/vmwathena_blockchain/docker
  gcloud compute ssh "${GCP_TARGET}" -- "
    cd ~/vmwathena_blockchain/docker &&
    ./make-prebuilt-env.sh > new.env &&
    mv new.env .env"
}

run_test () {
  # Must run ssh in tty mode to allow concord setup procedure to launch 
  # interactive docker using "docker run -it"
  gcloud compute ssh "$GCP_TARGET" -- -t "
    cd ~/vmwathena_blockchain &&
    export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG} &&
    export DAML_SDK_VERSION=${DAML_SDK_VERSION} &&
    daml/gcp/load-runner.sh test"
}

print_help () {
  echo "Usage: ./deploy.sh COMMAND" 
  echo ""
  echo "Available commands:"
  echo "help                     Show this help text"
  echo "images                   Deploy docker images"
  echo "scripts                  Deploy bash scripts"
  echo "test                     Run remote test"
  echo ""
  echo "This script requires that ssh target is provided in the GCP_TARGET environment variable:"
  echo "export GCP_TARGET=\"gcp_user_name_digitalasset_com@34.65.169.49\""
  echo ""
  echo "When running \"deploy.sh test\" you may additionally specify the desired DAML SDK as well"
  echo "as the spider version by using the DAML_SDK_VERSION and SPIDER_IMAGE_TAG environment variables."
}

## Define image version variables
export SPIDER_IMAGE_TAG=${SPIDER_IMAGE_TAG:-1.23.155}
export DAML_SDK_VERSION=${DAML_SDK_VERSION:-0.13.54}
export GCP_TARGET=${GCP_TARGET:?"Please provide ssh target: export GCP_TARGET=\"gcp_user_name_digitalasset_com@34.65.169.49\""}

OPERATION=${1:-"help"}

case $OPERATION in

  test)
    run_test
    ;;

  images)
    deploy_images
    ;;

  scripts)
    deploy_scripts
    ;;

  help)
    print_help
    ;;

esac
