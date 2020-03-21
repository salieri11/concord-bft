#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import sys
from functools import wraps

import trio

from itertools import cycle

if 'hermes_util' in sys.modules.keys():
    from hermes_util.tee.tutil import Tee
else:
    from util.tee.tutil import Tee


class ConcordSkvbcClient:
    """
    This class implements the same interface as a BFT client.
    However, the underlying implementation only talks to a single endpoint,
    via the higher-level TEE protocol (gRPC).

    This client is a workaround for running Apollo tests against
    production-like Concord deployments which do not (yet) support
    external BFT clients.
    """

    def __init__(self, host="localhost", port="50051",
                 client_id=os.path.basename(__file__)):
        self.tee = Tee(host, port, client_id)

    async def write(self, msg, seq_num=None):
        """
        Sends a serialized SKVBC write message to the TEE endpoint
        """
        return self.tee.skvbc_write(msg)

    async def read(self, msg, seq_num=None):
        """
        Sends a serialized SKVBC read message to the TEE endpoint
        """
        return self.tee.skvbc_read(msg)


def rotate_on_failure(
        async_fn, retry_interval_milli=500, req_timeout_milli=5000):
    """
    Retries the decorated method by rotating the target endpoint
    """
    @wraps(async_fn)
    async def wrapper(*args):
        client = args[0] # self of the decorated method
        with trio.fail_after(seconds=req_timeout_milli / 1000):
            while True:
                try:
                    with trio.fail_after(seconds=retry_interval_milli / 1000):
                        return await async_fn(*args)
                except Exception:
                    client._current_endpoint = next(client._endpoints)
                    continue
    return wrapper


class RotatingSkvbcClient:
    """
    Targets multiple SKVBC API endpoints. If a request fails, rotates its current endpoint.
    """
    def __init__(self, *args):
        assert len(args) > 0
        self._endpoints = cycle([ConcordSkvbcClient(port=p) for p in args])
        self._current_endpoint = next(self._endpoints)

    @rotate_on_failure
    async def write(self, msg, seq_num=None):
        return await self._current_endpoint.write(msg, seq_num)

    @rotate_on_failure
    async def read(self, msg, seq_num=None):
        return await self._current_endpoint.read(msg, seq_num)
