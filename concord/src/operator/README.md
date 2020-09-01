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
Go to `docker/config-public/dockerClientConfigInput.yaml` and set `num_of_participant_nodes: 1`, in addition, go to 
`docker/config-public/dockerConfigurationInput-daml-nano.yaml` and set `num_of_external_clients: 16`.
    
Now, generate the corresponding configuration files with
```
./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml-nano.yaml
./gen-docker-client-config.sh config-public/dockerClientConfigInput.yaml
```
# Start cluster

This starts 4 concord containers and their corresponding execution engines, along with 1 operator container.

`docker-compose -f docker-compose-daml-nano.yml -f docker-compose-operator.yml up`

# Supported Commands

Enter the operator container.
`docker exec -it docker_operator_1 bash`

## Test the operator setup
To test the operator setup (i.e. check the keys generation and the connection to the replicas), initiate a mock command
```
root@f441baea613f:/operator# ./concop mock
```
If everything went well the output will be
```$xslt
{'response': 'Hello From Reconfiguration Mock plugin', 'succ': True}
```
The above means that the request has gone through the consensus, validation and that the operator was able to receive at least 2f + 1 replies.