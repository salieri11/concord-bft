# Description
This script allows to run various tests and it calculates end to end  latencies histogram and the total throughput.
This folder contains:
1. `measure.py` - the main script
2. `tee_pb2*` - compiled `tee.proto`
3. `performance_pb2*` - compiled `performance.proto`
4. `histogram.py` - histogram component for measurements
5. `concord_pb2*` - compiled `concord.proto`
   
Current version doesn't support CLI parameters and requires manual parameteres changing in the `measure.py` file.

# Available tests
Currently script can run 3 tests:
1. send_write
   This script runs SKVBCWrite test (via Tee execution engine). Prerequisites for this test:
    * 4 concord containers should run with `tee_enable=true` parameter
    * Relevant parameters:
      * `num_of_requests` total num of request to be sent
      * `request_size` max request size
      * `block_key_size` key size 
      * `block_value_size` value size
  
2. send_write_tr
   This script runs test similar to SKVBCWrite but the data is sent with clientId=1 to be pushed to Thin Replica Client (TRC).
     * Assumes that the `trc-test-app:latest` image is present but the container may or may not be running.
       If the container doesn't run, the script will run it.
     * The `trc-test-app` Docker file , is located at `{root_project_folder}/thin-replica-client/DockerFileTestApp`
     * **IMPORTANT** The log level for the `trc-test-app` container should be set to `INFO`. The relevant file is 
       `{root_project_folder}/thin-replica-client/test-app/log4cplus.properties`. (`INFO` is the default setting).
     * 4 concord containers should run with `tee_enable=true` parameter
     * Relevant parameters:
      * `num_of_requests` total num of request to be sent
      * `num_of_updates` number of KV pairs that the block will contain
      * `block_key_size` key size for each KV pair
      * `block_value_size` value size for each KV pair
      * `trc_image_name` = TRC test app image name
      * `trc_container_name` = if running container manually, one can add -name "trc-test-app" and then to use it here
      * `thin_replica_client_id` = ** should be equal to the trc-test-app parameter ** client id for susbscribing to thin replica client

3. run_perf_batch
   This script runs performance write (via `Performance` execution engine). The script first sends command to the handler to create all future data in memory and then the execution phase just fetches the ready block from memory and writes it. See below explanation about `Performance Handler`
    * Assumes that 4 concord containers are running with `perf_enable=true` parameter (one may use the `{root_project_folder}/docker/config-public/dockerConfigurationInput-performance-handler.yaml` to generate configuration files)
    * Relevant parameters:
      * `num_of_requests` total num of request to be sent
      * `batch_test_kv_count` number of KV pairs that the block will contain
      * `batch_test_key_size` key size for each KV pair
      * `batch_test_value_size` value size for each KV pair
      * `batch_test_payload_size` = dummy payload size

## General script parameters:
  * `servers` list of Concord nodes in form of [(ip:port),...]
  * `clients_per_server` default ** EXTERNAL ** clients to be simulated (have nothing in common with Client Proxies)
  
  # Performance Handler
  The idea of having special Performance Handler comes from future plans to have an ability to control performance related system features at the runtime. Currently, it is used purely as Yet Another Execution Engine, but in future it can be used for various purposes, e.g. enabling/disabling performance oriented features etc.

  Current API is described in `performance.proto` and allows to send write requests with variable payload and different block size, which is more similar to production environments.

  It can be run in pre-init mode, where the blocks are created in replicas memory in advance (using the ExecInit method) and then in the execution phase the handler just writes ready block to the disk - it eliminates time needed to create data in memory to be written.

  Alternatively, each block can run produced "on the fly".
  In addition, the ExecClean method can be used to remove pre allocated data from the memory.
  
  See `performance.proto` for detailed service description.
            