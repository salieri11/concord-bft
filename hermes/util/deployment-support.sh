#!/bin/sh

################################################################################
# Use this to generate deployment support bundle of a concord node (replica)
#
# To run the script from a concord node, scp the script onto a concord node,
# and execute the script
# Usage: sh deployment-support.sh
#           --dockerContainers <list of expected docker containers to be running on concord node>
#
#           [Optional parameter to inclue concord public IP in support bundle tar.gz filename:
#              --concordIP <concord IP>
################################################################################

while [ "$1" != "" ] ; do
   case $1 in
      "--concordIP")
         shift
         CONCORD_IP="$1"
         ;;
      "--dockerContainers")
         shift
         CONCORD_CONTAINERS="$1"
         ;;
   esac
   shift
done

error() {
    TS=`date +"%Y-%m-%d_%H-%M-%S"`
    echo "$TS : =================================================" | tee -a $ERROR_LOG
    echo "$TS : $1" | tee -a $ERROR_LOG
    echo "$TS =================================================" | tee -a $ERROR_LOG
}

info() {
    TS=`date +"%Y-%m-%d_%H-%M-%S"`
    echo "$TS : $1" | tee -a $OUTPUT_LOG
}

usage() {
   if [ -z "${CONCORD_CONTAINERS}" ]
   then
      info "Usage: sh $0 <OPTIONS>"
      info "   --dockerContainers <list of expected docker containers to be running on concord node>"
      info "  [--concordIP <concord IP>]"
      
      exit 1
   fi
}

gather_docker_logs() {
   for container in `echo "${CONCORD_CONTAINERS}" | tr '[' ' ' | tr ']' ' ' | tr ',' ' '`
   do
      info "Gathering '${container}' log..."
      container_log="${SUPPORT_BUNDLE_PATH}/${container}.log"
      docker logs "${container}" > "${container_log}" 2>&1
   done
}

bundle_support_logs() {
   info "Installing 'tar' binary..."
   tdnf install -y tar
   RC=$?
   if [ ! "${RC}" = "0" ]
   then
      error "Failed to install 'tar' binary on the concord node"
      exit 1
   fi

   info "Bundling logs..."
   compressed_support_bundle="${SUPPORT_BUNDLE_BASE_PATH}/${SUPPORT_BUNDLE_DIR_NAME}.tar.gz"
   tar cfz "${compressed_support_bundle}" "${SUPPORT_BUNDLE_PATH}" "/config"
   RC="$?"

   if [ "${RC}" = "0" ]
   then
      info "Support bundle created successfully: ${compressed_support_bundle}"
   else
      error "Failed to create support bundle: ${compressed_support_bundle}"
      exit 1
   fi
}

usage
TIME_STAMP=`date +"%Y-%m-%d_%H-%M-%S"`
TMP="/tmp"
# SUPPORT_BUNDLE_DEFAULT_DIR_NAME="deployment-support-bundle"
if [ ! -z "${CONCORD_IP}" ]
then
   CONCORD_IP_STR=`echo "${CONCORD_IP}" | tr '.' '-'`
else
   CONCORD_IP_STR="concord"
fi
SUPPORT_BUNDLE_DIR_NAME="deployment-support-bundle_${CONCORD_IP_STR}_${TIME_STAMP}"
SUPPORT_BUNDLE_BASE_PATH="${TMP}"
SUPPORT_BUNDLE_PATH="${SUPPORT_BUNDLE_BASE_PATH}/${SUPPORT_BUNDLE_DIR_NAME}"
ERROR_LOG="${SUPPORT_BUNDLE_PATH}"/error.log
OUTPUT_LOG="${SUPPORT_BUNDLE_PATH}"/output.log

if [ ! -d "${SUPPORT_BUNDLE_PATH}" ]
then
   mkdir -p "${SUPPORT_BUNDLE_PATH}"
   info "Support bundle directory: ${SUPPORT_BUNDLE_PATH}"
fi

gather_docker_logs
bundle_support_logs


