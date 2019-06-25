# VMW Blockchain

This is the code for the VMware blockchain. It is broken into a few
components:

 * Concord: the blockchain replica node
 * EthRPC: a translator from Web3 JSON RPC to Concord protobuf
 * Helen: the API server
 * Hermes: the integration test framework
 * Persephone: the deployment engine / fleet management service
 * UI: the browser-based UI for interacting with Helen

## Skipping the Build

If you want to experiment with VMW Blockchain without building the
components yourself, please use the instructions found on (the Docker
page of the Blockchain Confluence
site)[https://confluence.eng.vmware.com/display/BLOC/Docker] to use
pre-built images produced by our CI/CD pipeline.

## Building via Docker

The easiest way to use this system is via docker. Please install the
(community edition)[https://docs.docker.com/install/linux/docker-ce/ubuntu/].

> Note: The build instructions listed here was verified with Docker
> 18.09+. There are known issues with the current set of build
> instructions with Docker on Linux at version 18.06 and possibly
> other prior versions as well. Please update Docker to 18.09+ before
> proceeding, regardless of the host platform, to minimize potential
> incompatibilities.

If you are running linux, you will also need to install
(docker-compose)[https://docs.docker.com/compose/install/]. If you're
running Mac OS or Windows, docker-compose was installed with docker.

### Sign into Artifactory

We have cached some of the build environments in docker images on
Artifactory. You will need access to those to run our build
scripts. So, ensure that you log your docker daemon into Artifactory
before continuing:

```
$ docker login -u <your vmware username without @vmware.com> \
               athena-docker-local.artifactory.eng.vmware.com
```

### Getting the Code

When you clone the repository, include `--recursive` or
`--recurse-submodules` to get the submodule dependencies as well:

```
$ git clone --recursive \
      git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git
```

If you cloned without recursing through submodules, you can set them
up by running the following commands in the root directory of your
cloned repository:

```
blockchain$ git submodule init
blockchain$ git submodule update
```

### Building everything at once

With docker installed, please run the "buildall.sh" script found in
the root directory of the repository:

```
blockchain$ ./buildall.sh
Loading repos/tags for docker images from docker/.env
Docker: /usr/bin/docker
Building...
Adding build process: Concord=84165
Adding build process: Concord_for_memleak=84166
Adding build process: UI=84167
Adding build process: Fluentd=84168
mvn-repo
Adding build process: Maven=84187
Waiting for maven build of helen/ethrpc/communication...
Waiting for maven build of helen/ethrpc/communication...
...
Adding build process: Ethrpc_docker_image=87731
Adding build process: Helen_docker_image=87732
Adding build process: Persephone_docker_image=87733
Adding build process: Cockroach_DB=87734
Adding build process: Reverse_proxy=87735
Adding build process: Asset_Transfer_sample_image=87736
Adding build process: Agent_docker_image=87737
Adding build process: Contract Compiler Microservice=87738
...
-------- Status --------
Ethrpc_docker_image: done
UI: done
Reverse_proxy: done
Agent_docker_image: done
Persephone_docker_image: done
Contract Compiler Microservice: done
Helen_docker_image: done
Fluentd: done
Concord: done
Asset_Transfer_sample_image: done
Concord_for_memleak: done
Maven: done
Cockroach_DB: done
```

This process will build all components, and tag each image with a
`:latest` tag.

```
bfink@ubuntu:~/vmwathena/blockchain-alt$ docker image ls | grep latest
persephone                                                         latest               f22f55ddc180        8 minutes ago       545MB
helen                                                              latest               f511a4f01c46        13 minutes ago      596MB
agent                                                              latest               88e8c8d07b29        13 minutes ago      571MB
ethrpc                                                             latest               10bfd0847e1b        13 minutes ago      556MB
contract-compiler                                                  latest               5f91b3834d95        13 minutes ago      737MB
...
```

If the script completes successfully, you can skip the rest of this
Building section, and start Testing. If any of the components failed
to build, or you want to rebuild one of them, please see the following
subsections for information on how to build each component
individually.

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

### Building Contract Compiler via Docker

The Contract Compiler is built directly in a docker container (note the command is
run from the same directory as this README, and not from the `contract-compiler`
subdirectory):

```
blockchain$ docker build contract-compiler -f contract-compiler/Dockerfile -t contract-compiler:latest
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
Please note this is just temporary.
Credentials for pushing to docker-hub are different from regular user.

```
blockchain$ docker build . -f agent/Dockerfile -t vmwblockchain/agent-testing:latest
```
If you want to push to the docker-hub:
```
docker push vmwblockchain/agent-testing
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
  suite                 Test suite name. Available suites: [
                        'ContractCompilerTests', 'CoreVMTests',
                        'LintTests', 'ExtendedRPCTests', 'HelenAPITests',
                        'PerformanceTests', 'RegressionTests', 'SampleDAppTests',
                        'SimpleStateTransferTest', 'TruffleTests', 'UiTests']
```
