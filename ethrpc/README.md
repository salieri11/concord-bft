# Ethereum RPC Handler (ethrpc)

This Blockchain sub-project is a service to translate Ethereum JSON RPC
requests into Concord requests. It is referred to in various places as
"the RPC Stub".

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images. The rest of this section explains how to build
EthRPC natively, which can be useful for debugging.

Ethrpc depends on the communication module, so you should have built
(../communication) before this.

Ethrpc is built using mvn:

```
mvn clean package
```

## Running

Please see [../README.md](README.md) for instructions on using
docker-compose to launch EthRPC in tandem with concord nodes. The rest
of this section explains how to launch EthRPC directly for debugging
purposes.

An EthRPC process connects to exactly one Concord node. Launch EthRPC
to connect to your chosen Concord node by specifying that node as the
`ConcordAuthorities` property. For example, to connect to Concord on
`localhost` and port `5458`, use:

```
java -jar target/concord-ethrpc*.jar --ConcordAuthorities=localhost:5458
```

Without the SSL properties found in (../docker/docker-compose.yml),
EthRPC will expose its API over unencrypted HTTP. See the examples in
the docker-compose definition for how to launch EthRPC with its API
exposed over HTTPS.

## SSL

Ethrpc connections use HTTPS. For now, we're using self-signed
certificates. An example certificate that will allow access via
localhost or 127.0.0.1 URLs is included in
[../docker/config-ethrpc1].

A matching `.pem` file that a client can use to trust the self-signed
certificate is included in [../docker/config-helen]. This file
is referenced from Helen's `application.properties`, and will be
served along with the RPC URL from Helen's `/api/concord/members`
endpoint.

To generate a new certificate, use this command:

```
keytool -genkeypair -alias ethrpc -keyalg RSA -keysize 2048 \
        -storetype PKCS12 -keystore keystore.p12 -validity 3650 \
        -ext san=ip:127.0.0.1
```

Please note the following:

1. In the command above, change `127.0.0.1` to the IP you will use in
URLs to access the ethrpc service.

2. The password you use must match that given in
[src/main/resources/application.properties] or
[../docker/docker-compose.yml].

3. Use the fully-qualified domain name you will use in URLs as the
answer to "What is your full name?". If testing locally, this is
probably "localhost".

After generating the new keys, extract the public `.pem` file with the
following command:

```
openssl pkcs12 -in keystore.p12 -out cacert.pem -clcerts -nokeys
```

You should now have two files:

1. `keystore.p12` contains the private key, and should be placed where
the ethrpc service can read it.

2. `cacert.pem` contains the public key, and should be placed where
Helen can read it.

## Using

The expected pattern for interacting with ethrpc goes like this:

### Step one: find ethrpc endpoints

Ask Helen where the ethrpc endpoints for your blockchain are:

Curl (just fetches the URL for the first member):

```
# note: the `-k` is not needed for PoC SaaS nodes
#       it is needed here because localhost != ds.blockchain.vmware.com
curl -k 'https://admin%40blockchain.local:Admin!23@localhost/blockchains/local/api/concord/members | jq .[0].rpc_url
```

Node:

```
const r = require("request");
const Web3 = require("web3");
const https = require('https');

const options = {
    // note: `rejectUnauthorized: false` is not needed for PoC SaaS nodes
    //       it is needed here because localhost != ds.blockchain.vmware.com
    rejectUnauthorized: false
};

let g_concord_providers = [];
const getContext = function() {
  console.log('Getting Context');
  r("https://admin@blockchain.local:Admin!23@localhost/blockchains/local/api/concord/members?certs=true",
    options,
    function(error, response) {
      if (error) { console.log(error); }
      else {
        const m = JSON.parse(response.body);
        if (!https.globalAgent.options.ca) {
          https.globalAgent.options.ca = [];
        }

        // purposefully global
        g_concord_providers = [];

        // Load each URL into a providers list, and add each cert to the list of CAs
        for (let i = 0; i < m.length; i++) {
          g_concord_providers.push(new Web3.providers.HttpProvider(m[i].rpc_url));
          https.globalAgent.options.ca.push(m[i].rpc_cert);
        }
      }
    });
};

getContext();
```

### Step two: download the public keys for the endpoint

You can skip this step if you're using Node. The instructions in Step
one saved the key along with the URL.

Curl (just fetches the cert for the first member):

```
curl -k https://blockchains/local/api/concord/members | jq -r .[0].rpc_cert > cacert.pem
```

## Step three: use the URL and the cert

Curl:

```
curl -v --cacert cacert.pem https://127.0.0.1:8545/
```

Node:

```
// assuming getContext code, above, was run in the same console
const w3 = new Web3(g_concord_providers[0]);
w3.eth.getGasPrice(
  function(error, price) {
    if (error) { console.log("ERROR: ", error); }
    else { console.log("PRICE: ", price); }
  });
w3.eth.getBlock(0,
  function(error, block, z) {
    if (error) { console.log("ERROR: ", error); }
    else { console.log("BLOCK: ", block); }
  });

// or, directly, without web3:
r(g_concord_providers[0].host,
  function(error, response, body) {
    if (error) { console.log("ERROR: ", error); }
    else { console.log("response: ", response); }
  });
```

Truffle:

```
TODO
```
