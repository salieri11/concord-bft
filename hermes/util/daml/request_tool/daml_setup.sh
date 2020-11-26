#!/bin/bash

#name of the host
ledger_host="${1}"
#port of the host
ledger_port="${2}"
#path where root certificates  and key are present
path="${3}"
#flag to check for certificates
test_type="${4}"
echo "${path}"
echo "${ledger_host}"
cd "$path" || exit


daml_build()
{
~/.daml/bin/daml build
}
daml_list_with_crt()
{
~/.daml/bin/daml ledger list-parties --host "${ledger_host}" --port "${ledger_port}" --cacrt root-ca.crt --pem client.key --crt client.crt
}
daml_list_without_crt()
{
~/.daml/bin/daml ledger list-parties --host "${ledger_host}" --port "${ledger_port}"
}
daml_allocate_with_crt()
{
~/.daml/bin/daml ledger allocate-parties --host "${ledger_host}" --port "${ledger_port}" --cacrt root-ca.crt --pem client.key --crt client.crt
}
daml_allocate_without_crt()
{
~/.daml/bin/daml ledger allocate-parties --host "${ledger_host}" --port "${ledger_port}"
}
daml_upload_with_crt()
{
~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port "${ledger_port}" ~/.daml/dist/ledgerclient-0.0.1.dar --cacrt root-ca.crt --pem client.key --crt client.crt
}

daml_upload_without_crt()
{
~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port "${ledger_port}" ~/.daml/dist/ledgerclient-0.0.1.dar
}


create_invalid_client_crt()
{
create_root_crt
create_client_crt
}

if [ "$test_type" == "with_certificates" ]
then
  daml_list_with_crt
elif [ "$test_type" == "without_certificates" ]
then
  daml_list_without_crt
elif [ "$test_type" == "with_invalid_cert" ]
then 
  daml_list_with_crt
else
  error "**** Type not valid"
fi