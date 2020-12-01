#!/bin/bash

#name of the host
ledger_host="${1}"
#port of the host
ledger_port="${2}"
#path where root certificates  and key are present
cert_path="${3}"
#parameters
parameter="${4}"

echo "${cert_path}"
echo "${ledger_host}"
cd "$cert_path" || exit


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
~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port "${ledger_port}" "${dar_path}" --cacrt root-ca.crt --pem client.key --crt client.crt
}

daml_upload_without_crt()
{
~/.daml/bin/daml ledger upload-dar --host "${ledger_host}" --port "${ledger_port}" "${dar_path}"
}


create_invalid_client_crt()
{
create_root_crt
create_client_crt
}

if [ "$parameter" == "with_certificates" ]
then
  daml_list_with_crt
elif [ "$parameter" == "without_certificates" ]
then
  daml_list_without_crt
elif [ "$parameter" == "with_invalid_cert" ]
then 
  cd "invalid"
  daml_list_with_crt
elif [ "$parameter" == "upload_with_crt" ]
then 
  dar_path="${5}"
  echo $dar_path
  if [ -f "$dar_path" ]
  then
    daml_upload_with_crt
  else
	  error "***********Dar file doesn't exist"
  fi
elif [ "$parameter" == "daml_test_crt" ]
then
test_type="${6}"
test_jar="${7}"
java -jar "${test_jar}" --include "${test_type}" --timeout-scale-factor 20 --no-wait-for-parties --cacrt root-ca.crt --pem client.key --crt client.crt --concurrent-test-runs 1 "${ledger_host}":"${ledger_port}"
else
  error "**** Type not valid"
fi