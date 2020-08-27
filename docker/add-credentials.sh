#!/bin/bash

# ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ████████╗ ██████╗
# ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ╚══██╔══╝██╔═══██╗
# ██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗         ██║   ██║   ██║
# ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝         ██║   ██║   ██║
# ╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗       ██║   ╚██████╔╝
#  ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝       ╚═╝    ╚═════╝

#  ██████╗██████╗ ███████╗██████╗ ███████╗███╗   ██╗████████╗██╗ █████╗ ██╗
# ██╔════╝██╔══██╗██╔════╝██╔══██╗██╔════╝████╗  ██║╚══██╔══╝██║██╔══██╗██║
# ██║     ██████╔╝█████╗  ██║  ██║█████╗  ██╔██╗ ██║   ██║   ██║███████║██║
# ██║     ██╔══██╗██╔══╝  ██║  ██║██╔══╝  ██║╚██╗██║   ██║   ██║██╔══██║██║
# ╚██████╗██║  ██║███████╗██████╔╝███████╗██║ ╚████║   ██║   ██║██║  ██║███████╗
#  ╚═════╝╚═╝  ╚═╝╚══════╝╚═════╝ ╚══════╝╚═╝  ╚═══╝   ╚═╝   ╚═╝╚═╝  ╚═╝╚══════╝

#  ██████╗ ██████╗ ███╗   ██╗███████╗██╗ ██████╗
# ██╔════╝██╔═══██╗████╗  ██║██╔════╝██║██╔════╝
# ██║     ██║   ██║██╔██╗ ██║█████╗  ██║██║  ███╗
# ██║     ██║   ██║██║╚██╗██║██╔══╝  ██║██║   ██║
# ╚██████╗╚██████╔╝██║ ╚████║██║     ██║╚██████╔╝
#  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝ ╚═════╝


#
# This script is for populating Auth credentials and Config parameters
#
# Steps:
# 1. Please have an admin add you the VMware Last Pass Account
# 2. Install Last Pass CLI. https://github.com/lastpass/lastpass-cli
# 3. Run the script ./add-credentials.sh
# 4. Then add your lastpass password when prompted
#

if [ -z "$VAULT_API_TOKEN" ];
	then
		echo "Your Vault Token is not set.  Please set to proceed.";
		echo ""
		echo "How to setup:"
		echo "1. Get the Vault API token from vault admin"
		echo "2. Add 'export VAULT_API_TOKEN=<VAULT_API_TOKEN>' to your .bashrc or whichever bash profile you use."
		echo "3. 'source ~/.bashrc' in your terminal"
		echo "4. Run the script ./add-credentials.sh"
		echo ""

		exit 1;
fi
version=${1:-1}

get_value() {
  name=$1
  key=$2
	RESPONSE=`curl -s --header "X-Vault-Token: ${VAULT_API_TOKEN}" http://10.78.20.9:8200/v1/kv/data/$name?version=$version | \
    python3 -c "import sys, json; resp = json.load(sys.stdin); data = resp.get('data', {}).get('data'); value = data.get('$key'); print(value)"`

	echo "$RESPONSE"
}

CSP_PROD_REFRESH_TOKEN=`get_value 'CSP' 'PRODUCTION_API_TOKEN'`
CSP_STAGE_REFRESH_TOKEN=`get_value 'CSP' 'STAGING_API_TOKEN'`

BINTRAY_PW=`get_value 'bintray' 'dev_key'`
BINTRAY_UN=`get_value 'bintray' 'dev_user'`

DOCKER_R_ADDRESS=https://registry-1.docker.io/v2
DOCKER_R_UN=`get_value 'docker-reader' 'username'`
DOCKER_R_PW=`get_value 'docker-reader' 'password'`

CSP_STG_LOGIN_PW=`get_value 'csp-stage-dev-cred' 'password'`
CSP_STG_LOGIN_UN=`get_value 'csp-stage-dev-cred' 'username'`

IPAM_CERT=`get_value 'ipam' 'server_crt'`
test=$IPAM_CERT | tr " " "\n"
# echo $IPAM_CERT | tr " " "\n"
LOGINSIGHT_CLOUD_AUTH_BEARER=`get_value 'log-insight-cloud' 'key'`
LOGINSIGHT_ONPREM_USERNAME=`get_value 'log-insight-onprem' 'username'`
LOGINSIGHT_ONPREM_PASSWORD=`get_value 'log-insight-onprem' 'password'`

VCENTER_PW=`get_value 'VMware-Blockchain-SDDC-4' 'password'`
VCENTER_UN=`get_value 'VMware-Blockchain-SDDC-4' 'username'`

WAVEFRONT_API_TOKEN=`get_value 'wavefront-api' 'key'`

# Update user config
USER_CONFIG=../hermes/resources/user_config.json
sed -i '' -e 's/'"<DASHBOARD_WAVEFRONT_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $USER_CONFIG
sed -i '' -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_USERNAME>"'/'"${LOGINSIGHT_ONPREM_USERNAME}"'/g' $USER_CONFIG
sed -i '' -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_PASSWORD>"'/'"${LOGINSIGHT_ONPREM_PASSWORD}"'/g' $USER_CONFIG

# Update zone config
# TODO: This is only using one set of credentials for all SDDC configs. Didn't want to expose
# all of the credentials for all SDDCs.
ZONE_CONFIG=../hermes/resources/zone_config.json
sed -i '' -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<VMC_API_TOKEN>"'/'"${CSP_PROD_REFRESH_TOKEN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $ZONE_CONFIG
sed -i '' -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${LOGINSIGHT_CLOUD_AUTH_BEARER}"'/g' $ZONE_CONFIG

# Update app properties
APP_PROP=../hermes/resources/persephone/provisioning/app/profiles/application-test.properties
sed -i '' -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $APP_PROP
sed -i '' -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $APP_PROP
sed -i '' -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $APP_PROP

# Update Helen DB Zones
HELEN_ZONE_SQL_PATH=./config-helen/app/db/migration/R__zone_entities.sql
sed -i '' -e 's/'"<VMC_API_TOKEN>"'/'"${CSP_PROD_REFRESH_TOKEN}"'/g' $HELEN_ZONE_SQL_PATH
sed -i '' -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${LOGINSIGHT_CLOUD_AUTH_BEARER}"'/g' $HELEN_ZONE_SQL_PATH
sed -i '' -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $HELEN_ZONE_SQL_PATH

# Update Helen Application Properties
HELEN_PROP_PATH=./config-helen/app/profiles/application-test.properties
sed -i '' -e 's/'"<VMC_STAGE_API_TOKEN>"'/'"${CSP_STAGE_REFRESH_TOKEN}"'/g' $HELEN_PROP_PATH
sed -i '' -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $HELEN_PROP_PATH

# Update UI E2E tests
UI_CRED_PATH=../ui/e2e/credentials.json
sed -i '' -e 's/'"<CREDENTIALS_NOT_INJECTED_FROM_JENKINS>"'/'"set"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<UI_E2E_CSP_LOGIN_USERNAME>"'/'"${CSP_STG_LOGIN_UN}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<UI_E2E_CSP_LOGIN_PASSWORD>"'/'"${CSP_STG_LOGIN_PW}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${BINTRAY_UN}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${BINTRAY_PW}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_USERNAME>"'/'"${LOGINSIGHT_ONPREM_USERNAME}"'/g' $UI_CRED_PATH
sed -i '' -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_PASSWORD>"'/'"${LOGINSIGHT_ONPREM_PASSWORD}"'/g' $UI_CRED_PATH

# Update IPAM
echo "$IPAM_CERT" > ../hermes/resources/persephone/provisioning/ipam.crt
echo "$IPAM_CERT" > ./config-persephone/persephone/provisioning/ipam.crt

# Add persephone config for docker deploy
APP_PROP2=./config-persephone/app/profiles/application-test.properties
sed -i '' -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $APP_PROP2
sed -i '' -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $APP_PROP2
sed -i '' -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $APP_PROP2

PERSEPHONE_PROP_PATH=./config-persephone/persephone/provisioning/config.json
sed -i '' -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $PERSEPHONE_PROP_PATH
sed -i '' -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $PERSEPHONE_PROP_PATH
sed -i '' -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $PERSEPHONE_PROP_PATH
