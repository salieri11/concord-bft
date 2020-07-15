#!/bin/bash

# shellcheck disable=SC2155
set_values() {
  export ledger_id="KVBC"

  export workflow_id=$(uuidgen)
  export application_id="IouApp"
  export command_id=$(uuidgen)
  export party="Alice"

  export package_id="72656b4c7196dc6ecc0d9f1405834ef810022efdc42ef851e57fe6b5971ba303" # corresponds to sdk-version: 1.2.0
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
