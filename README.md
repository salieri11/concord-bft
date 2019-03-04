# VMW Blockchain

This is the code for the VMware blockchain. It is broken into a few
components:

 * Concord: the blockchain replica node
 * EthRPC: a translator from Web3 JSON RPC to Concord protobuf
 * Helen: the API server
 * Hermes: the integration test framework
 * UI: the browser-based UI for interacting with Helen

## Skipping the Build

If you want to experiment with VMW Blockchain without building the
components yourself, please use the instructions found on (the Docker
page of the Blockchain Confluence
site)[https://confluence.eng.vmware.com/display/BLOC/Docker] to use
pre-built images produced by our CI/CD pipeline.

## Building via Docker

The easiest way to use this system is via docker. Please install the
(community edition)[https://www.docker.com/community-edition]. If you
are running linux, you will also need to install
(docker-compose)[https://docs.docker.com/compose/install/]. If you're
running Mac OS or Windows, docker-compose was installed with docker.

### Building Concord via Docker

*Note*: If you did not include `--recursive` or `--recurse-submodules`
in your original git-clone instruction, you will need to run
`git submodule init && git submodule update` before building concord.
If you do not, the build will fail with an error about
`/concord/submodules/concord-bft` not containing a CMakeLists.txt file.

Concord can be built directly in a docker container (note the command
is run from the same directory as this README, and *not* from the
`concord` subdirectory):

```
blockchain$ docker build -f concord/Dockerfile . -t concord-core:latest
```

### Building EthRPC via Docker

EthRPC can be built directly in a docker container (note the command
is run from the same directory as this README, and *not* from the
`ethrpc` subdirectory):

```
blockchain$ docker build . -f ethrpc/Dockerfile -t ethrpc:latest
```

### Building Helen via Docker

Helen can be built directly in a docker container (note the command
is run from the same directory as this README, and *not* from the
`helen` subdirectory):

```
blockchain$ docker build . -f helen/Dockerfile -t helen:latest
```

### Building Hermes via Docker

Hermes does not need to be built before being used. See notes later
for how to use Hermes to test your images.

### Building UI via Docker

The UI is built directly in a docker container (note the command is
run from the same directory as this README, and not from the `ui`
subdirectory):

```
blockchain$ docker build ui -f ui/Dockerfile -t ui:latest
```

### Building Fluentd

Fluentd is build directly in a docker container (note the command is
run from the same directory as this README, and not from
`docker/fluentd`:

```
blockchain$ docker build docker/fluentd -f docker/fluentd/Dockerfile -t fluentd:latest
```

### Building Agent via Docker

concord-agent can be built directly in a docker container (note the command
is run from the same directory as this README, and *not* from the
`agent` subdirectory):

```
blockchain$ docker build . -f agent/Dockerfile -t ethrpc:latest
```


### Building Natively

Each component can also be built outside of Docker. For instructions
on how to do this, please see the README file in each project
subdirectory. For the Java-based subprojects, the following
"Alternative" instructions for using a dockerized Maven can also be
helpful.

#### Alternative Build Steps for Maven-build Components

The easiest way to build all Maven-build components (currently
`communication`, `ethrpc`, and `helen`) without installing any
toolchain is to trigger the build within a Docker container.
In addition, to leverage the build cache so subsequent builds
can be faster, it is advisable to create a container volume to
house all incrementally-built and downloaded artifacts. This
section will walk through step-by-step on how to go from ground
zero to having the relevant Docker images built.

Step 1: Create the Docker volume.

Here we create a container volume named `mvn-repo`.
```
blockchain$ docker volume create --name mvn-repo
```

Step 2: Invoke top-level build to trigger all sub-builds via Docker.

Here we create a container named `mvn-build`, with current directory
(blockchain/) and the `mvn-repo` container volume mounted. The
container starts the entry-point at `/workspace` which is mounted
to our host's current directory. Finally `mvn` command is triggered
to build and archive all the built artifacts into the `mvn-repo`
volume.

```
blockchain$ docker run \
              --rm --name mvn-build \
              -v maven-repo:/root/.m2 \
              -v "$(pwd)":/workspace \
              -w /workspace maven:3.6.0-jdk-11 \
              mvn clean install
```

Step 3: Create the Docker images.

This step just performs the equivalent sub-steps in the default build
steps (i.e. non-alternative) to create the relevant docker images.

```
blockchain$ docker build ethrpc -f ethrpc/packaging.Dockerfile -t ethrpc:latest
blockchain$ docker build helen -f helen/packaging.Dockerfile -t helen:latest
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
