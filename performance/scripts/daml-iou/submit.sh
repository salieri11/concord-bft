#!/bin/bash

# shellcheck disable=SC2155
set_values() {
  export ledger_id="KVBC"

  export workflow_id=$(uuidgen)
  export application_id="IouApp"
  export command_id=$(uuidgen)
  export party="Alice"
  export ledger_effective_time=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
  export maximum_record_time=$(date -v +15S -u +"%Y-%m-%dT%H:%M:%SZ")

  export package_id="9b6430501726001a5c5ecc829b48c8588640dd0309d24516b43bf75bb3f4fe3d"
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
