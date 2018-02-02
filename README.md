# VMWare Project Athena UI and API

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
``

With Erlang and Crypt++ installed, clone the repo:

```
$ git clone git@github.com:beerriot/helen && cd helen
```

Build using `make`:

```
helen$ make all rel
```

And start the app:

```
helen$ ./_build/default/rel/helen/bin/helen console
```

## Project Layout

The project is a
[Webmachine](https://github.com/webmachine/webmachine/wiki)
application. The resources currently defined are:

 * / : src/helen_resource.erl. A placeholder that will eventually
   display the landing page of the UI.

 * /api : src/helen_api_base_resource.erl. The base of the REST API. A
   GET request will serve the Swagger/OpenAPI specification for the
   service.

 * /api/eth : src/helen_api_eth_resource.erl. A JSON RPC endpoint
   compatible with [Ethereum's JSON RPC
   API](https://ethereum.gitbooks.io/frontier-guide/content/rpc.html),
   that exposes the Athena EVM-compatible blockchain.

Other files you'll find interesting:

 * /priv/swagger.json : The Swagger specification for the service.

 * /config/* : Configuration for the application.

 * /src/helen_config.erl : URL dispatch definitions and IP/Port
   binding.

## Components

Files in priv/www/swagger are from the
(swagger-ui)[https://github.com/swagger-api/swagger-ui] project,
Copyright 2018 SmartBear Software, Licensed under the Apache License,
Version 2.0.
