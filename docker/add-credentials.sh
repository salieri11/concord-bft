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

if ! lpass -v COMMAND &> /dev/null;
	then
		echo "Last Pass CLI is not installed.  Please install to proceed.";
		echo ""
		echo "How to setup:"
		echo "1. Have an admin add you the VMware Blockchain LastPass Account"
		echo "2. Install LastPass CLI. https://github.com/lastpass/lastpass-cli"
		echo "3. Run the script ./add-credentials.sh"
		echo "4. Add your LastPass password when prompted"
		echo ""

		exit 1;
fi

CSP_PROD_REFRESH_TOKEN=`lpass show --password 'CSP PROD Refresh Token'`
CSP_STAGE_REFRESH_TOKEN=`lpass show --password 'VMC CSP STAGE'`

BINTRAY_PW=`lpass show --password 'Bintray Reader'`
BINTRAY_UN=`lpass show --username 'Bintray Reader'`

DOCKER_R_ADDRESS=https://registry-1.docker.io/v2
DOCKER_R_UN=`lpass show --username 2314744336663705164`
DOCKER_R_PW=`lpass show --password 2314744336663705164`

CSP_STG_LOGIN_PW=`lpass show --password 'CSP Stage'`
CSP_STG_LOGIN_UN=`lpass show --username 'CSP Stage'`

IPAM_CERT=`lpass show --notes 429127633260185028`

LOGINSIGHT_CLOUD_AUTH_BEARER=`lpass show --password 'Log Insight Cloud'`
LOGINSIGHT_ONPREM_PASSWORD=`lpass show --password 'Log Insight On Prem'`

VCENTER_PW=`lpass show --password 'vCenter'`
VCENTER_UN=`lpass show --username 'vCenter'`
VMC_API_TOKEN=`lpass show --username 'VMC_API_TOKEN'`

WAVEFRONT_API_TOKEN=`lpass show --password 'Wavefront API'`

# Update user config
USER_CONFIG=../hermes/resources/user_config.json
sed -i -e 's/'"<DASHBOARD_WAVEFRONT_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $USER_CONFIG
sed -i -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_PASSWORD>"'/'"${LOGINSIGHT_ONPREM_PASSWORD}"'/g' $USER_CONFIG

# Update zone config
# TODO: This is only using one set of credentials for all SDDC configs. Didn't want to expose
# all of the credentials for all SDDCs.
ZONE_CONFIG=../hermes/resources/zone_config.json
sed -i -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<VMC_API_TOKEN>"'/'"${CSP_PROD_REFRESH_TOKEN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $ZONE_CONFIG
sed -i -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${LOGINSIGHT_CLOUD_AUTH_BEARER}"'/g' $ZONE_CONFIG

# Update app properties
APP_PROP=../hermes/resources/persephone/provisioning/app/profiles/application-test.properties
sed -i -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $APP_PROP
sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $APP_PROP
sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $APP_PROP

# Update Helen DB Zones
HELEN_ZONE_SQL_PATH=./config-helen/app/db/migration/R__zone_entities.sql
sed -i -e 's/'"<VMC_API_TOKEN>"'/'"${CSP_PROD_REFRESH_TOKEN}"'/g' $HELEN_ZONE_SQL_PATH
sed -i -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${LOGINSIGHT_CLOUD_AUTH_BEARER}"'/g' $HELEN_ZONE_SQL_PATH
sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $HELEN_ZONE_SQL_PATH

# Update Helen Application Properties
HELEN_PROP_PATH=./config-helen/app/profiles/application-test.properties
sed -i -e 's/'"<VMC_STAGE_API_TOKEN>"'/'"${CSP_STAGE_REFRESH_TOKEN}"'/g' $HELEN_PROP_PATH
sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $HELEN_PROP_PATH

# Update UI E2E tests
UI_CRED_PATH=../ui/e2e/credentials.json
sed -i -e 's/'"<CREDENTIALS_NOT_INJECTED_FROM_JENKINS>"'/'"set"'/g' $UI_CRED_PATH
sed -i -e 's/'"<UI_E2E_CSP_LOGIN_USERNAME>"'/'"${CSP_STG_LOGIN_UN}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<UI_E2E_CSP_LOGIN_PASSWORD>"'/'"${CSP_STG_LOGIN_PW}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_USERNAME>"'/'"${VCENTER_UN}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_PASSWORD>"'/'"${VCENTER_PW}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${BINTRAY_UN}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${BINTRAY_PW}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' $UI_CRED_PATH
sed -i -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_PASSWORD>"'/'"${LOGINSIGHT_ONPREM_PASSWORD}"'/g' $UI_CRED_PATH

# Update IPAM
echo "$IPAM_CERT" > ../hermes/resources/persephone/provisioning/ipam.crt
echo "$IPAM_CERT" > ./config-persephone/persephone/provisioning/ipam.crt

# Add persephone config for docker deploy
APP_PROP2=./config-persephone/app/profiles/application-test.properties
sed -i -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $APP_PROP2
sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $APP_PROP2
sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $APP_PROP2

PERSEPHONE_PROP_PATH=./config-persephone/persephone/provisioning/config.json
sed -i -e 's%'"<CONTAINER_REGISTRY_ADDRESS>"'%'"${DOCKER_R_ADDRESS}"'%g' $PERSEPHONE_PROP_PATH
sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKER_R_UN}"'/g' $PERSEPHONE_PROP_PATH
sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKER_R_PW}"'/g' $PERSEPHONE_PROP_PATH
