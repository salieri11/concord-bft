#!/usr/bin/env bash


#!/usr/bin/env bash

# creates simple self signed certificates to use with TCP TLS module

if [ "$#" -eq 0 ] || [ -z "$1" ]; then
   echo "usage: create_certs.sh {num of replicas} {optional - output folder}"
   exit 1
fi

dir=$2
if [ -z $dir ]; then
   dir="certs"
fi

if [ ! -d $dir ]; then
   mkdir $dir
fi

i=0
#//Generate DH parameters
openssl ecparam -name secp384r1 -out $dir/dh_srv.pem
openssl ecparam -name secp384r1 -out $dir/dh_cl.pem

while [ $i -lt $1 ]; do
   echo "processing replica $i"
   clientDir=$dir/$i/client
   serverDir=$dir/$i/server

   mkdir $dir/$i
   mkdir $clientDir
   mkdir $serverDir

   openssl ecparam -in $dir/dh_srv.pem -genkey -noout -out \
$serverDir/pk.pem
   openssl ecparam -in $dir/dh_cl.pem -genkey -noout -out \
$clientDir/pk.pem

   openssl req -new -key $serverDir/pk.pem -nodes -days 365 -x509 \
-subj "/C=NA/ST=NA/L=NA/O=NA/OU=node"$i"/CN=node"$i"ser" -out $serverDir/server.cert

   openssl req -new -key $clientDir/pk.pem -nodes -days 365 -x509 \
-subj "/C=NA/ST=NA/L=NA/O=NA/OU=node"$i"/CN=node"$i"cli" -out $clientDir/client.cert

   let i=i+1
done

exit 0
