# VMW Blockchain

This is the code for the VMware blockchain. It is broken into a few
components:

 * Concord: the blockchain replica node
 * Helen: the UI and API server
 * Hermes: the integration test framework

## Building

The easiest way to use this system is via docker. Please install the
(community edition)[https://www.docker.com/community-edition]. If you
are running linux, you will also need to install
(docker-compose)[https://docs.docker.com/compose/install/]. If you're
running Mac OS or Windows, docker-compose was installed with docker.

You will need to build images for both helen and concord.

### Building Prereqs

Some utilities are shared among the projects, and they must be built
first:

```
blockchain/communication$ mvn clean install
```

### Building Helen

Helen is first build using Maven (see helen/README.md for additional
dependencies, like Java and NPM):

```
blockchain/helen$ mvn clean install package
```

Now, build the docker image, and tag it `helen:latest`:

```
blockchain/helen$ docker build . -t helen:latest
```

### Building Concord

Concord can be built directly in a docker container (note the command
is run from the same directory as this README, and *not* from the
`concord` subdirectory):

```
blockchain$ docker build -f concord/Dockerfile . -t concord:latest
```

## Testing

Once your containers are built, run the tests using hermes:

```
blockchain/hermes$ sudo ./main.py TEST_NAME
```

Replace TEST_NAME with any of the suites available:

```
blockchain/hermes$ ./main.py --help
usage: main.py [-h] [--ethereumMode] [--logLevel LOGLEVEL]
               [--resultsDir RESULTSDIR] [--tests TESTS] [--config CONFIG]
               [--dockerComposeFile DOCKERCOMPOSEFILE] [--noLaunch]
               [--keepconcordDB] [--repeatSuiteRun REPEATSUITERUN]
               suite

positional arguments:
  suite                 Test suite name. Available suites: ['CoreVMTests',
                        'ExtendedRPCTests', 'HelenAPITests',
                        'PerformanceTests', 'KVBlockchainTests',
                        'RegressionTests']
```
