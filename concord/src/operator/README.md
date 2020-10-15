# TESTING

Make sure you've built concord so you can use it for generating configurations

```bash
# Enter the concord root of the repo
cd ~/vmwathena_blockchain/concord

# Ensure you have the latest code built outside the concord image for fast rebuilds
make

# Build the image
cd .. # Root vmwathena_blockchain directory
docker build -f concord/Dockerfile -t concord-core:latest .

# Build the operator image
docker build -f concord/src/operator/Dockerfile -t operator:latest .

```

Everything below this line starts from the root docker directory. Replace with the location of your repo.

`cd ~/vmwathena_blockchain/docker`

## Setup your docker environment

Use the remote images for everything that matches your build.
`./make-prebuilt-env.sh > temp.env && mv temp.env .env`
Change the first two lines of `.env` to point to your local concord build.
```
concord_repo=concord-core
concord_tag=latest
```

## Create participant node configs


## Create configuration files

Generate the corresponding configuration files with
in `config-public/dockerConfigurationInput-reconfiguration.yaml` comment out `key_view_path: /tmp`
Then, type the following:
```
./gen-docker-concord-config.sh config-public/dockerConfigurationInput-reconfiguration.yaml
./gen-docker-operator-config.sh config-public/dockerOperatorConfigInput.yaml
```
# Start cluster

This starts 4 concord containers and their corresponding execution engines, along with 1 operator container.

`docker-compose -f docker-compose-daml-nano.yml -f docker-compose-operator.yml up`

# Supported Commands

Enter the operator container.
`docker exec -it docker_operator_1 bash`

## Test the operator setup
Below we describe the current supported commands.
Type `./concop` and then ont of the following commands:

| Command | Action | Output |
|-------|------|------|
| release download [version] | Download SW version to the replicas (not implemented yet) | `{'additional_data': 'Upgrading', 'succ': True}` or `{'succ': False}`|
| release status | Check the installation status (not implemented yet) | `{'additional_data': 'Valid', 'succ': True}` or `{'succ': False}`|
| release install [version] | install the SW version on the replicas (not implemented yet) | `{'additional_data': 'Upgraded', 'succ': True}` or  `{'succ': False}`|
| wedge stop | Stop all replicas on the next next checkpoint | `{'additional_data': 'set stop flag', 'succ': True}` or `{'succ': False}`|
| wedge status | Check the wedge status of the replicas | list of `replica_id : wedge_status`|
