#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import sys
import struct

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
