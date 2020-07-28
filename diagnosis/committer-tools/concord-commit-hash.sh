#!/bin/bash

source ./ssh-exec.sh

SCRIPT_NAME="/tmp/concord-commit-hash-script.sh"
cat <<EOS > ${SCRIPT_NAME}
#!/bin/bash

LATEST_CONCORD_IMAGE=\$(docker images | grep "concord-core" | awk '{print \$1 ":" \$2}' | sort | tail -1)
if [ -z "\${LATEST_CONCORD_IMAGE}" ]; then
    >&2 echo "Coulnd't find concord docker image"
    exit 1
fi

hash=\$(docker image inspect \${LATEST_CONCORD_IMAGE} | jq '.[0].ContainerConfig.Labels["com.vmware.blockchain.commit"]')
if [ "\${hash}" != "null" ]; then
  hash=\${hash%\"}
  hash=\${hash#\"}
  echo \${hash}
fi
EOS

function concord_commit_hash {
  if [ -z "$1" ]; then IP=${HOST_IP}; else IP=$1; fi
  ssh_exec_script "${IP}" "${SCRIPT_NAME}"
}

[[ ${BASH_SOURCE[0]} = $0 ]] && concord_commit_hash $@
