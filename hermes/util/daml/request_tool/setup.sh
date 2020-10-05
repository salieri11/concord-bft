#!/bin/bash

host="${1}"

if [ -z "${host}" ]
then
    host=localhost
fi

# This script is meant to ease test case development.
# It builds the project, allocates parties, and uploads the dar, all
# to the local sandbox ledger.
daml build
daml ledger allocate-parties --host "${host}" --port 6865
daml ledger upload-dar --host "${host}" --port 6865 .daml/dist/ledgerclient-0.0.1.dar
