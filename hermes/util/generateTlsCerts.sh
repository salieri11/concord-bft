#!/usr/bin/env bash             

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
commonName_default = systest.ledgerapi.com

[v3_req]
basicConstraints = CA:true
keyUsage = critical, keyCertSign
EOL

openssl req -x509 -new -newkey rsa:2048 -nodes -keyout "root-ca.key" -out "root-ca.crt" \
  -days 3650  -subj '/C=US/ST=CA/L=San Francisco/O=Docker/CN=systest.ledgerapi.com' -config "root-ca.cnf" -extensions v3_req 



openssl genrsa -out "server.key.rsa" 2048

openssl pkcs8 -topk8 -in "server.key.rsa" -out "server.key" -nocrypt 


openssl req -new -key "server.key" -out "server.csr" -sha256 \
        -subj '/C=US/ST=CA/L=San Francisco/O=Docker/CN=systest.ledgerapi.com'

cat > "server.cnf" <<EOL
[server]
authorityKeyIdentifier=keyid,issuer
basicConstraints = critical,CA:FALSE
extendedKeyUsage=serverAuth,clientAuth
keyUsage = critical, digitalSignature, keyEncipherment
subjectAltName = DNS:systest.ledgerapi.com, DNS:localhost, IP:127.0.0.1
subjectKeyIdentifier=hash
EOL

openssl x509 -req -days 365 -in "server.csr" -sha256 \
        -CA "root-ca.crt" -CAkey "root-ca.key"  -CAcreateserial \
        -out "server.crt" -extfile "server.cnf" -extensions server

rm "server.cnf" "server.csr" "root-ca.cnf" "root-ca.srl" "server.key.rsa"