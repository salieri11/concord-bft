#!/bin/bash

# shellcheck disable=SC2155
set_values() {
  export ledger_id="KVBC"

  export workflow_id=$(uuidgen)
  export application_id="IouApp"
  export command_id=$(uuidgen)
  export party="Alice"
  export ledger_effective_time=$(date -I'seconds')
  export maximum_record_time=$(date --date='15 seconds' -I'seconds')

  export package_id="164d14178afe1dc4de698c70390e054407a270ba7d9882f8a3b6c1950d019640" # corresponds to sdk-version: 0.13.46
  export module_name="Iou"
  export entity_name="Iou"

  export issuer="Alice"
  export owner="Alice"
  export currency="AliceCoin"
  export amount=$RANDOM
  export observer="Alice"
}

main() {
  set_values

  host=$1
  port=6865
  service=com.digitalasset.ledger.api.v1.CommandService
  method=SubmitAndWaitForTransactionId

  envsubst <request.json | grpcurl -plaintext -d @ "$host":"$port" "$service"/"$method"
}

# Validate
if [ $# -eq 0 ]; then
  echo "usage: $0 <IP>"
  exit 1
fi

main "$1"
