#!/bin/bash

ledger_host="${1}"
ledger_port="${2}"
path="${3}"
cd path
echo ${path}
echo ${ledger_host}
if [ -z "${ledger_host}" ]
then
    ledger_host=localhost
fi

# This script is meant to ease test case development.
# It builds the project, allocates parties, and uploads the dar, all
# to the local sandbox ledger.
~/.daml/bin/daml ledger list-parties --host "${ledger_host}" --port "${ledger_port}" --cacrt root-ca.crt --pem client.key --crt client.crt
# ~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port 6865 .daml/dist/ledgerclient-0.0.1.dar --cacrt root-ca.crt --pem client.key --crt client.crt