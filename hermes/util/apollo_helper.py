#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import sys
import subprocess
import trio

from functools import wraps

import hermes_util.hermes_logging as hermes_logging_util

log = hermes_logging_util.getMainLogger()


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
