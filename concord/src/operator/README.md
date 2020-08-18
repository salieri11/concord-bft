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

`./gen-docker-client-config.sh config-public/dockerClientConfigInput.yaml`

## Create concord configs

`./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml-nano.yaml`

## Add 2 more TLS cert files for the operator

```
../concord/submodules/concord-bft/scripts/linux/create_tls_certs.sh 52
mv certs/50 tls_certs/
mv certs/51 tls_certs/
rm -rf certs
```

# Start cluster

This starts 4 concord containers and their corresponding execution engines, along with 1 operator container.

`docker-compose -f docker-compose-daml-nano.yml -f docker-compose-operator.yml up`

# Show the current status

Enter the operator container.
`docker exec -it docker_operator_1 bash`

Check the status. This is currently just a dummy call.
`./concop release status`
