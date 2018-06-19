# VMware Project Athena UI and API

Helen is Athena's inter*face to launch a thousand* requests. This
repository will be the home of the API server for Project
Athena. Helen is a Java Undertow server which invokes different
servlets for different apis. Note : Helen will run on port 8080 of
localhost by default.

### Prerequisites

[Java 8](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)

[Maven](https://www.rosehosting.com/blog/how-to-install-maven-on-ubuntu-16-04/)

[Athena](https://github.com/vmwathena/athena)

[Solidity compiler] Install solidity compiler (`solc`) with version >= 0.4.20
for `ubuntu` solidity compiler can be installed using `sudo apt-get install solc`

[CockroachDB] Install cockroachDB on the same machine as helen.
A simple configuration guide for CockroachDB is provided in `cockroachdb_setup.md`.

[Protocol Buffers v2.x](https://developers.google.com/protocol-buffers/docs/proto)


### macOS Setup

Install Java from the link above in the prerequisites, but maven can
be installed via brew.

```
brew install boost aclocal protobuf@2.5 automake autoconf maven
```

Boost and protobuf won't be automatically linked

```
export DYLD_LIBRARY_PATH=/usr/local/Cellar/boost/1.66.0/lib:$DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/usr/local/Cellar/protobuf@2.5/2.5.0/lib:$DYLD_LIBRARY_PATH
```

### Buiding and running the UI

You will need Node installed to build the UI. The project targets the
current LTS version of Node (8.9.x). You can install Node directly, or
by using NVM.

If you are not utilizing NVM, please skip any commands that reference it.

Before completing any of the following commands, make sure you are in
the webapp/ directory and have the correct version of Node activated:

```
cd webapp/
nvm use
```

Also required is the Angular CLI, which currently handles asset
compilation, component generation, and provides a local development
server. Install using the following commands:

```
npm install -g @angular/cli
```

Install all dependencies through NPM.

```
npm install
```

Build and copy the UI in development mode to the priv/www/ directory
for use on a locally running server:

```
npm run build:local:dev
```

Build and copy the UI in production mode to the priv/www/ directory
for use on a locally running server:

```
npm run build:local:prod
```

Start the UI in watch mode, typically used for local development. This will:

    Start a server listening on http://localhost:4200 to deliver the UI assets.

    Proxy all API requests to an existing Helen server. Edit webapp/proxy.conf.json and make sure target matches your desired server and port.

    Watch for file changes under the webapp/ and compile automatically.

```
npm start
```

### Building and Running Helen

 * [Run Athena](https://github.com/vmwathena/athena).

 * Install dependencies and build using maven.

   To build only with unit tests (No Athena instance is needed) :
```
mvn clean install package
```

   To build with unit and integration tests : (Athena must be running for this)
```
mvn clean install -DskipIntegrationTests=false
```

   To build without rebuilding the UI :
```
mvn clean install -Dskip.npm
```

 * Run the server

```
java -jar helen-1.0-SNAPSHOT-jar-with-dependencies.jar
```

### API

 * `/swagger/*` - Used to serve the static content from priv/www/swagger

 * `/assets/*` - Used to serve static content from priv/www/assets

 * `/api/` - Used to return a list of all other APIs serviced

 * `/api/athena/*` - API endpoints (see
   [swagger](https://github.com/vmwathena/helen/blob/master/priv/swagger.json))

 * `/*` - Used to serve content from priv/www/index.html

### Using the UI

 * Point your browser to localhost:8080 to load the dashboard.

 * Point your browser to localhost:8080/swagger/index.html to load the
   swagger UI.

### Configurations

Server and servlet configurations are stored in a key-value format in
[config.properties](https://github.com/vmwathena/helen/blob/master/config.properties).
Note : Helen needs to be restarted if any changes are made to the
config.properties file.

### Latency

The latency of the ecosystem (Helen + Athena) can be tested by using a
tool called
[ApacheBench](https://httpd.apache.org/docs/2.4/programs/ab.html).

 * Install using steps found
   [here](https://kuntalchandra.wordpress.com/2015/10/10/install-apache-bench-ubuntu-14-04/).

 * To fire GET requests, use the command `ab -t 60 -n 10000000 -c 10
   {request url}`.  This command fires the specified GET request for
   60 seconds (`-t` flag) or 10000000 times (`-n` flag) (whichever
   comes first). The `-c` flag is used to set the number of concurrent
   requests being fired at all times.

 * To fire POST requests, use the command `ab -p {filename}.txt -T
   application/json -t 60 -n 10000000 {request url}` In addition to
   the parameters in the GET request, this requires a `-p` flag. A
   filename must be provided which contains the parameters of the POST
   request.

   Sample file contents :

```
   {"jsonrpc":"2.0","method":"eth_sendTransaction","params":[{
   "from": "0xb60e8dd61c5d32be8058bb8eb970870f07233155",
   "to": "0xd46e8dd67c5d32be8058bb8eb970870f07244567",
   "gas": "0x76c0",
   "gasPrice": "0x9184e72a000",
   "value": "0x9184e72a",
   "data": "0xd46e8dd67c5d32be8d46e8dd67c5d32be8058bb8eb970870f072445675058bb8eb970870f072445675"
   }],"id":1}
```

### Components

Files in priv/www/swagger are from the
[swagger-ui](https://github.com/swagger-api/swagger-ui) project,
Copyright 2018 SmartBear Software, Licensed under the Apache License,
Version 2.0.

A previous version of Helen was written in Erlang. It is archived on
the [bwf-erlang
branch](https://github.com/vmwathena/helen/tree/bwf-erlang) in case
there were mistakes in the transition.
