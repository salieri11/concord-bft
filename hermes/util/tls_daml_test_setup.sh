#!/bin/bash

ledger_host="${1}"
path="${2}"
echo ${path}
echo ${ledger_host}
if [ -z "${ledger_host}" ]
then
    ledger_host=localhost
fi

# This script is meant to ease test case development.
# It builds the project, allocates parties, and uploads the dar, all
# to the local sandbox ledger.
~/.daml/bin/daml ledger list-parties --host "${ledger_host}" --port 6865 --cacrt "${path}"root-ca.crt --pem "${path}"client.key --crt "${path}"client.crt
~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port 6865 .daml/dist/ledgerclient-0.0.1.dar --cacrt "${path}"root-ca.crt --pem "${path}"client.key --crt "${path}"client.crt