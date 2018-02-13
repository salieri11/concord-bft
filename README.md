# VMware Project Athena UI and API

Helen is Athena's inter*face to launch a thousand* requests. This
repository will be the home of the UI and API server for Project
Athena.

## Building

You will need [Erlang](http://www.erlang.org/) installed to build this
repository. This application has been tested on Erlang/OTP R18. If
you're using Ubuntu, you can install what you need from apt:

```
$ apt-get install erlang
```

You will also need Crypto++, version 5.6.4 or later. To install:

```
git clone https://github.com/weidai11/cryptopp.git
cd cryptopp/
git checkout CRYPTOPP_5_6_5;
mkdir build/
cd build/
cmake ..
make
sudo make install
```

With Erlang and Crypt++ installed, clone the repo:

```
$ git clone git@github.com:beerriot/helen && cd helen
```

Build using `make`:

```
helen$ make all rel
```

## Using

After you have built Helen, you can start it like so:

```
helen$ ./_build/default/rel/helen/bin/helen console
```

That will set up an HTTP listener on port 8080. Point your browser to
http://localhost:8080/ and you should see a sparse landing page with
links to a Swagger viewer and a proof-of-concept dashboard.

At this point, you can also attach an Ethereum console:

```
$ geth attach http://localhost:8080/api/athena/eth
```

In order to actually send transactions, you'll also need a
[P2_Blockchain](https://github.com/guyg7/P2_Blockchain) EVM cluster
running. Once you have that, edit `sys.config` and set the IP and port
for each `Blockchain_client` instance. This config file is located in
the `config` directory, and the default build process symlinks
`_build/default/rel/helen/releases/<version>/sys.config` to it.

The `eth_sendTransaction` method will assume you're creating a
contract if you omit the `to` field from the `params` structure. It
will generate an address for the contract and return that.

The `eth_sendRawTransaction` method expects the `data` field in the
`params` list is 0x-encoded data of the following format:

 * characters 0,1: type of transaction. "01" for create, "02" for call.

 * characters 2-41: "to" parameter. The address at which the contract
   should live (for create) or does live (for call).

 * characters 42-81: "from" parameter. Address of who is creating or
   calling the contract.

 * characters 82-161: "endowment/value" parameter.

 * characters 162-end: "data" parameter. The code of the contract (for
   create) or the argument to it (for call).

## Project Layout

The project is a
[Webmachine](https://github.com/webmachine/webmachine/wiki)
application. The resources currently defined are:

 * /api : src/helen_api_base_resource.erl. The base of the REST API. A
   GET request will serve the Swagger/OpenAPI specification for the
   service.

 * /api/athena/eth : src/helen_api_eth_resource.erl. A JSON RPC
   endpoint compatible with [Ethereum's JSON RPC
   API](https://ethereum.gitbooks.io/frontier-guide/content/rpc.html),
   that exposes the Athena EVM-compatible blockchain.

 * /api/athena/members :
   src/helen_api_member_list_resource.erl. Serves a JSON list of
   information about nodes currently making up the cluster.

 * anything else: files served from priv/www by
   src/helen_static_resource.erl

Other files you'll find interesting:

 * priv/swagger.json : The Swagger specification for the service.

 * config/* : Configuration for the application. Of particular
   interest: the addresses for the P2_Blockchain clients to connect to
   are specified here.

 * src/helen_config.erl : URL dispatch definitions and IP/Port
   binding.

 * test/* : EUnit tests, run automatically during the build

## Components

Files in priv/www/swagger are from the
[swagger-ui](https://github.com/swagger-api/swagger-ui) project,
Copyright 2018 SmartBear Software, Licensed under the Apache License,
Version 2.0.
