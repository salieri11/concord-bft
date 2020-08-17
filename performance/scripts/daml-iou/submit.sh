#!/bin/bash

# shellcheck disable=SC2155
set_values() {
  export ledger_id="KVBC"

  export workflow_id=$(uuidgen)
  export application_id="IouApp"
  export command_id=$(uuidgen)
  export party="Alice"

  export package_id="cef433031945004576f3d173fa15ea04ebc689fcdca236e0d036b5d9c0591c7b" # corresponds to sdk-version: 1.4.0
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
  service=com.daml.ledger.api.v1.CommandService
  method=SubmitAndWaitForTransactionId

  envsubst <request.json | grpcurl -plaintext -d @ "$host":"$port" "$service"/"$method"
}

# Validate
if [ $# -eq 0 ]; then
  echo "usage: $0 <IP>"
  exit 1
fi

main "$1"
