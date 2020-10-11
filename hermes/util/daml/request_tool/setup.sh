#!/bin/bash

ledger_host="${1}"

if [ -z "${ledger_host}" ]
then
    ledger_host=localhost
fi

# This script is meant to ease test case development.
# It builds the project, allocates parties, and uploads the dar, all
# to the local sandbox ledger.
daml build
daml ledger allocate-parties --host "${ledger_host}" --port 6865
daml ledger upload-dar --host "${ledger_host}" --port 6865 .daml/dist/ledgerclient-0.0.1.dar
