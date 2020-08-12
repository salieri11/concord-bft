#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import sys
import subprocess
import trio

from functools import wraps
from bft import BftTestNetwork, TestConfig
from bft_config import Replica
from hermes_util.skvbc.concord_external_client import ExternalBftClient

import hermes_util.hermes_logging as hermes_logging_util

log = hermes_logging_util.getMainLogger()

async def create_bft_network():
    config = TestConfig(n=4,
                        f=1,
                        c=0,
                        num_clients=4,
                        key_file_prefix=None,
                        start_replica_cmd=start_replica_cmd,
                        stop_replica_cmd=stop_replica_cmd,
                        num_ro_replicas=0)
    replicas = [Replica(id=i, ip="127.0.0.1", port=3501 + i, metrics_port=4501 + i)
                for i in range(config.num_clients)]
    clients = [ExternalBftClient(i) for i in range(config.num_clients)]
    bft_network = BftTestNetwork.existing(config, replicas, clients, lambda client_id: ExternalBftClient(client_id))
    return bft_network

def start_replica_cmd(builddir, replica_id):
    log.info(f"Starting replica #{replica_id}")
    return ["docker", "start", f"docker_concord{replica_id + 1}_1"]


def stop_replica_cmd(replica_id):
    log.info(f"Stopping replica #{replica_id}")
    return ["docker", "kill", f"docker_concord{replica_id + 1}_1"]


def with_timeout(async_fn):
    """
    Decorator that makes sure an async test case doesn't run indefinitely.
    This could happen sometimes due to a co-routine waiting indefinitely for something to happen
    (f.e. a view change).

    In case the timeout is reached, we print the list of live Concord containers, to help debug such cases.
    """

    @wraps(async_fn)
    async def timeout_wrapper(*args, **kwargs):
        try:
            with trio.fail_after(seconds=5 * 60):
                return await async_fn(*args, **kwargs)
        except trio.TooSlowError:
            raw_output = subprocess.check_output(["docker", "ps", "--format", "{{ .Names }}"])
            live_containers = [c.decode('utf-8') for c in raw_output.split()]
            log.info(f"Live containers at the time of failure: {live_containers}")
            raise

    return timeout_wrapper
