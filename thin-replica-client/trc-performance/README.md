# TRC-TRS Performance Benchmark

The TRC-TRS performance benchmark has two components, a mocked client located at `/thin-replica-client/trc-performance`, and mocked server which is located at `/concord/tools/trs_performance`.

## Run the TRC-TRS Performance Benchmark

The benchmark can be run using docker-compose as follows:
```
docker$ ./gen-docker-concord-config.sh config-public/dockerConfigurationInput-daml-nano.yaml
docker$ sudo docker-compose -f docker-compose-perf-trc-trs.yml up -d
```

## Updating Benchmark Input Parameters

The input parameters i.e., the characteristics of dummy data to be written to
rocksdb can be updated in the docker compose file.

Currently the following parameters can be configured by the user.
```
NUM_BLOCKS: number of blocks to be sent, each time fakestorage is filled
NUM_KVP: number of key value pairs to be sent per block
KEY_SIZE: size of each key in kilobytes
VAL_SIZE: size of each value in kilobytes
```
