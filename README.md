# VMware Project Athena UI and API

Helen is Athena's inter*face to launch a thousand* requests. This
repository will be the home of the UI and API server for Project
Athena.

There are two ways to get started developing in this repo. If you will
be modifying server-side code, follow the instructions in the
''Building and Running Directly'' section. If you will only be
modifying browser-side code, you can instead use the instructions in
the ''Building and Running with Docker'' section.

## Building and Running Directly

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

After you have built Helen, you can start it like so:

```
helen$ ./_build/default/rel/helen/bin/helen console
```

You'll find Helen's landing page by pointing your browser at
`http://localhost:8080/`. Access and error logs can be found in
`_build/default/rel/helen/log/`.

## Building and Running with Docker

This project can be built and run in docker containers. First, you
will need an image. A pre-built one is available in a private Docker
Hub repository,
[beerriot/p2bc](https://hub.docker.com/r/beerriot/p2bc/) (ping Bryan
for access). You can alternately clone and build using the Dockerfile
in this repository.

That is, this step is optional:

```
$ git clone git@github.com:vmwathena/helen
$ cd helen
$ docker build .
...
 ---> 18d1585da8e1
 Successfully built 18d1585da8e1
```

Use either the hash from your build, or a tag from the Docker Hub repo
(e.g. `beerriot/p2bc:helen20180213`) in your run command.

Either:

```
$ docker container run -it 18d1585da8e1
```

Or:

```
$ docker container run -it beerriot/p2bc:helen20180213
```

The `-it` is important. The application currently starts a console,
and it will fail if the container is not interactive. A future TODO is
to get this running in detached mode, or scrolling the access log
instead of showing the console.

On Mac, also add -P to the argument list, to expose ports.

Find out where the container is hosted:

```
$ docker container ls
CONTAINER ID        IMAGE                         COMMAND                  CREATED                  STATUS              PORTS                     NAMES
facc12edfdce        beerriot/p2bc:helen20180213   "/bin/sh -c 'export …"   Less than a second ago   Up 18 seconds       0.0.0.0:32769->8080/tcp   quizzical_wing
```

Point your browser to the IP:Port specified (here `0.0.0.0:32769`) and
you should see the project landing page. You can view the access and
error logs using the following commands:

```
$ docker container exec facc12edfdce ls /helen/log
access.log.2018_02_13_22
wm_error.log.2018_02_13_22
$ $ docker container exec facc12edfdce tail /helen/log/access.log.2018_02_13_22
172.17.0.1 - - [13/Feb/2018:22:15:32 +0000] "GET / HTTP/1.1" 200 352 "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:58.0) Gecko/20100101 Firefox/58.0"
172.17.0.1 - - [13/Feb/2018:22:15:32 +0000] "GET /favicon.ico HTTP/1.1" 404 193 "" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:58.0) Gecko/20100101 Firefox/58.0"
```

Using the simple commands above, you'll see whatever was in the repo
at the time of build. If you would like to modify the files that your
instance of Helen is serving, clone the repository and mount the
`priv/www` directory in the container like so:

```
~$ git clone git@github.com:vmwathena/helen
~$ docker container run -it --mount type=bind,source=$HOME/helen/priv/www,destination=/helen/lib/helen-1/priv/www,readonly beerriot/p2bc:helen20180213
```

Now when you edit the static files in your repo at
`~/helen/priv/www/`, those changes will be served by your Helen
instance, without rebuilding or restarting. (Note: edits to files
elsewhere in the project will not be reflected in the running
instance.)

## Using

Once you have Helen running, pointing a browser at its exposed port
will show you a sparse landing page with links to a Swagger viewer and
a proof-of-concept dashboard (note that the host and port will be
different if you used docker).

```
$ open http://localhost:8080/
```

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

 * /api/* : src/helen_404_resource.erl. Reserved for future expansion.
 
 * /swagger/* : files served from priv/www/swagger/ by
   src/helen_static_resource.erl
   
 * /assets/* : files served from priv/www/assets/ by
   src/helen_static_resource.erl
   
 * anything else: priv/www/index.html served by
   src/helen_static_resource.erl. This is to allow the UI to be
   implemented as an Angular single-page application. Which view to
   display is chosen client-side, based on the URL, but the base
   application HTML is always the same.

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
