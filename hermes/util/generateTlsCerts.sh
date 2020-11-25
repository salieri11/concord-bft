#!/bin/bash

create_root_crt()
{
        cat > "root-ca.cnf" <<EOL
[req]
distinguished_name  = req_distinguished_name
req_extensions = v3_req

[req_distinguished_name]
countryName           = Country Name (2 letter code)
countryName_default = US
stateOrProvinceName   = State or Province Name (full name)
stateOrProvinceName_default = CA
organizationName          = Organization Name (eg, company)
organizationName_default = Docker
commonName            = Common Name (eg, YOUR name)
commonName_default = *.ledgerapi.com

[v3_req]
basicConstraints = CA:true
keyUsage = critical, keyCertSign
EOL

        openssl req -x509 -new -newkey rsa:2048 -nodes -keyout "root-ca.key" -out "root-ca.crt" \
  -days 3650  -subj '/C=US/ST=CA/L=San Francisco/O=Docker/CN=*.ledgerapi.com' -config "root-ca.cnf" -extensions v3_req

        rm  "root-ca.cnf" 
}

create_server_crt()
{
        openssl genrsa -out "server.key.rsa" 2048

        openssl pkcs8 -topk8 -in "server.key.rsa" -out "server.key" -nocrypt


        openssl req -new -key "server.key" -out "server.csr" -sha256 \
                -subj '/C=US/ST=CA/L=San Francisco/O=Docker/CN=server.ledgerapi.com'

        cat > "server.cnf" <<EOL
[server]
authorityKeyIdentifier=keyid,issuer
basicConstraints = critical,CA:FALSE
extendedKeyUsage=serverAuth,clientAuth
keyUsage = critical, digitalSignature, keyEncipherment
subjectAltName = DNS:server.ledgerapi.com, DNS:localhost, IP:127.0.0.1
subjectKeyIdentifier=hash
EOL

        openssl x509 -req -days 365 -in "server.csr" -sha256 \
                -CA "root-ca.crt" -CAkey "root-ca.key"  -CAcreateserial \
                -out "server.crt" -extfile "server.cnf" -extensions server
        rm "server.cnf" "server.csr"  "server.key.rsa"
}

create_client_crt()
{
        openssl genrsa -out "client.key.rsa" 2048

        openssl pkcs8 -topk8 -in "client.key.rsa" -out "client.key" -nocrypt


        openssl req -new -key "client.key" -out "client.csr" -sha256 \
                -subj '/C=US/ST=CA/L=San Francisco/O=Docker/CN=client.ledgerapi.com'

        cat > "client.cnf" <<EOL
[client]
authorityKeyIdentifier=keyid,issuer
basicConstraints = critical,CA:FALSE
extendedKeyUsage=serverAuth,clientAuth
keyUsage = critical, digitalSignature, keyEncipherment
subjectAltName = DNS:client.ledgerapi.com, DNS:localhost, IP:127.0.0.1
subjectKeyIdentifier=hash
EOL

        openssl x509 -req -days 365 -in "client.csr" -sha256 \
                -CA "root-ca.crt" -CAkey "root-ca.key"  -CAcreateserial \
                -out "client.crt" -extfile "client.cnf" -extensions client
        rm "client.cnf" "client.csr" "client.key.rsa"
}


type="${1}"

if [ "$type" == "root_ca" ]
then
  create_root_crt
elif [ "$type" == "server" ]
then
  create_server_crt
elif  [ "$type" == "client" ]
then
  create_client_crt
else
  error "**** Type $1 not valid"
fi
