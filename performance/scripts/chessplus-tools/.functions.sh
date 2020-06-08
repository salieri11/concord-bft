#!/bin/bash

# SSH without prompting for password.
_do_ssh() {
  local login=$1
  local command=$2
  sshpass -p "$SSHPASS" ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" -o "PubkeyAuthentication no" "$login" "$command"
}

# SCP without prompting for password.
_do_scp() {
  local passwd=$1
  local source=$2
  local target=$3
  sshpass -p "$passwd" scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" -o "PubkeyAuthentication no" -r "$source" "$target"
}

# Find all committer nodes.
_find_committers() {
  local login="$SSHUSER@$SSHHOST"
  local file="/config/daml-ledger-api/environment-vars"
  local command="sed -r -n 's/^export REPLICAS=(\S+)$/\1/p' $file | sed 's/:50051//g; s/,/ /g'"
  _do_ssh "$login" "$command"
}

# Find given tag in telegraf config.
_find_tag() {
  local name=$1
  local login="$SSHUSER@$SSHHOST"
  local file="/config/telegraf/telegraf.conf"
  local command="sed -r -n 's/  $name = \"(\S+)\"$/\1/p' $file"
  _do_ssh "$login" "$command"
}

# Find log file path.
_log_path() {
  local login=$1
  local container=$2
  local command="docker inspect -f='{{.LogPath}}' $container"
  _do_ssh "$login" "$command"
}

# SCP container logs.
_scp_log() {
  local node=$1
  local container=$2
  local login="$SSHUSER@$node"

  local src_path=$(_log_path "$login" "$container")
  local dst_path="./$BASE_PATH/logs/$node/$container"

  mkdir -p "$dst_path"
  # Remove asterisk to avoid collecting rolled logs.
  _do_scp "$SSHPASS" "$login:$src_path*" "$dst_path"
}

# SCP relevant container logs from all the nodes.
bundle_logs() {
  _scp_log "$SSHHOST" daml_ledger_api
  _scp_log "$SSHHOST" daml_index_db

  for committer in $REPLICAS; do
    _scp_log "$committer" concord
    _scp_log "$committer" daml_execution_engine
  done
}

# Zip logs after converting from JSON to text.
zip_logs() {
  for i in ./"$BASE_PATH"/logs/*/*/*-json.log*; do
    local ext=$(basename "$i" | cut -d '.' -f 2,3)
    local name=$(basename "$(dirname "$i")")
    local dir=$(dirname "$i")
    jq -r -j .log "$i" | gzip >"$dir/$name.$ext.gz"
    rm "$i"
  done
}

# Move all reports to the bundle
bundle_reports() {
  mkdir -p ./"$BASE_PATH"/reports/
  mv load-runner-* "$_"
}

# Upload logs and reports.
upload_bundle() {
  local source=$(dirname "./$BASE_PATH")
  local target=$APACHE_SERVER_USERNAME@$APACHE_SERVER_HOST:/mnt/newlog/$APACHE_SERVER_BASE_DIR
  _do_scp "$APACHE_SERVER_PASSWORD" "$source" "$target"
}

# Dynamic variables.
init_env() {
  WAVEFRONT_SOURCE=$(_find_tag "source")
  BLOCKCHAIN_ID=$(_find_tag "blockchain")
  CONSORTIUM_ID=$(_find_tag "consortium")

  VM_IP=$(hostname -I | cut -d ' ' -f 1)
  REPLICAS=$(_find_committers)
  BASE_PATH=$BLOCKCHAIN_ID/$(date +%Y-%m-%d_%H-%M_%Z)
}

# Start docker-compose
start-sidecars() {
  # Default owner is root if created by container.
  mkdir -p "./$BASE_PATH/logs"
  docker-compose up -d
}

# stop docker-compose
stop-sidecars() {
  docker-compose down
}

# Send slack message
slack_msg() {
  local file="$1"
  envsubst <"$file" | curl -X POST -H 'Content-type: application/json' --data @- "$SLACK_WEBHOOK_URL"
}

# shellcheck disable=SC2155
