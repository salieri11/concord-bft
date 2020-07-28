#!/bin/bash

source ./concord-container-status.sh
source ./num-concord-coredumps.sh
source ./replica-ip-to-id.sh
source ./replica-ips.sh
source ./ssh-exec.sh
source ./ssh-probe.sh

if [ -z "${SSHPASS}" ]; then
  >&2 echo "Please set SSHPASS"
  exit 1
fi

TMP_DIR=/tmp/diagnosis
mkdir ${TMP_DIR} 2>/dev/null
IP_LIST=$(replica_ips)

# Start all SSH queries in parallel
for ip in ${IP_LIST}; do
  TMP="${TMP_DIR}/${ip}"
  mkdir ${TMP} 2>/dev/null

  ssh_probe ${ip} 2>/dev/null
  if [ $? -ne 0 ]; then
    echo "no" > ${TMP}/ssh
    echo "n/a" > ${TMP}/concord
    echo "n/a" > ${TMP}/root
    echo "n/a" > ${TMP}/core
    continue
  fi
  echo "yes" > ${TMP}/ssh

  concord_container_status ${ip} > ${TMP}/concord &
  ssh_exec ${ip} "df -h | grep root | awk '{print \$5}'" > ${TMP}/root &
  num_concord_coredumps ${ip} > ${TMP}/core &
done

printf "REPLICA\tIP\t\tSSH\tCONCORD\t/root\tCOREDUMPS\n"

wait

for ip in ${IP_LIST}; do
  TMP="${TMP_DIR}/${ip}"
  printf "%s\t" $(replica_ip_to_id ${ip})
  printf "%s\t" ${ip}
  printf "%s\t" $(cat ${TMP}/ssh)
  printf "%s\t" $(cat ${TMP}/concord)
  printf "%s\t" $(cat ${TMP}/root)
  printf "%s\n" $(cat ${TMP}/core)
done

rm -rf ${TMP_DIR}
