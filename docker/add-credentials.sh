#!/bin/bash

# This script is for populating auth credentials and config parameters

# Auth
read -p "Enter VMware Username: "  UN
read -s -p "Enter VMware Password: "  PSWD

# Paths
JENKINS_BASE_PATH=https://blockchain.svc.eng.vmware.com/
BUILD_ID=`curl --user ${UN}:${PSWD} ${JENKINS_BASE_PATH}/job/Master%20Branch%20Blockchain%20Run%20on%20GitLab/job/master/lastSuccessfulBuild/buildNumber`
# Base path for successful JENKINS master runs
MASTER_BRANCH_PATH=${JENKINS_BASE_PATH}/job/Master%20Branch%20Blockchain%20Run%20on%20GitLab/job/master/${BUILD_ID}/artifact/blockchain

# Credential paths
declare -a AUTH_PATHS=("docker/config-helen/app/db/migration/R__zone_entities.sql"
	"hermes/resources/user_config.json"
	"hermes/resources/zone_config.json"
	"hermes/resources/persephone/provisioning/app/profiles/application-test.properties"
	# "hermes/resources/persephone/provisioning/config.json"
	# "hermes/resources/persephone/provisioning/ipam.crt"
	"docker/config-persephone/app/profiles/application-test.properties"
	"ui/e2e/credentials.json"
)

for path in ${AUTH_PATHS[@]}; do
	echo "Adding credentials from ${path}"
	curl -X POST -L --user ${UN}:${PSWD} ${MASTER_BRANCH_PATH}/${path} > ../${path}
done
