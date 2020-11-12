mkdir config-local
mkdir core
mkdir rocksdb_out
if [[ $# -eq 1 ]] ; then
  cp ./config-public/application$1.config ./config-local/application.config
  cp ./config-public/deployment$1.config ./config-local/deployment.config
  cp ./config-public/secrets$1.config ./config-local/secrets.config
else
  cp ./config/bftclient.config ./config-local/bftclient.config
fi
cp ./config-public/log4cplus.properties ./config-local/log4cplus.properties
chmod ugo+r -R tls_certs
chmod ugo+r -R trs_trc_tls_certs
