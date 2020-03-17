# Testing DAML on VMware in GCP environment

This script assumes that the docker `latest` images required to run DAML on VMware are
all present in the local docker repository of the target GCP system. It also assumes that
docker login has been provided that allows for pulling spider artefacts from the DA's docker
hub.

To test specific spider version set the `SPIDER_IMAGE_TAG` environment variable to the
desired version. To test a specific DAML SDK version set `DAML_SDK_VERSION` variable.

Launch the script from the main `vmwathena_blockchain` directory:

```vmwathena_blockchain>daml/gcp/load-runner.sh```
