#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import struct

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

    def write(self, msg, seq_num=None):
        """
        Sends a serialized SKVBC write message to the TEE endpoint
        """
        return self.tee.skvbc_write(msg)

    def read(self, msg, seq_num=None):
        """
        Sends a serialized SKVBC read message to the TEE endpoint
        """
        return self.tee.skvbc_read(msg)


def test_skvbc_client():
    SKVBC_READ = 1
    SKVBC_WRITE = 2

    SKVBC_KV_LEN = 21
    SKVBC_READ_LATEST = 0xFFFFFFFFFFFFFFFF

    def raw_write_req(readset, writeset, block_id):
        data = bytearray()
        # A conditional write request type
        data.append(SKVBC_WRITE)
        # SimpleConditionalWriteHeader
        data.extend(
            struct.pack("<QQQ", block_id, len(readset), len(writeset)))
        # SimpleKey[numberOfKeysInReadSet]
        for r in readset:
            data.extend(r)
        # SimpleKV[numberOfWrites]
        for kv in writeset:
            data.extend(kv[0])
            data.extend(kv[1])

        return data

    def raw_read_req(readset, block_id=SKVBC_READ_LATEST):
        data = bytearray()
        data.append(SKVBC_READ)
        # SimpleReadHeader
        data.extend(struct.pack("<QQ", block_id, len(readset)))
        # SimpleKey[numberOfKeysToRead]
        for r in readset:
            data.extend(r)
        return data

    def parse_read_reply(reply):
        data = reply[1:]
        num_kv_pairs = struct.unpack("<Q", data[0:8])[0]
        data = data[8:]
        kv_pairs = {}
        for i in range(num_kv_pairs):
            kv_pairs[data[0:SKVBC_KV_LEN]] = data[SKVBC_KV_LEN:2 * SKVBC_KV_LEN]
            if i + 1 != num_kv_pairs:
                data = data[2 * SKVBC_KV_LEN:]
        return kv_pairs

    from collections import namedtuple
    WriteReply = namedtuple('WriteReply', ['success', 'last_block_id'])
    def parse_write_reply(reply):
        data = reply[1:]
        return WriteReply._make(struct.unpack("<?Q", data))

    client = ConcordSkvbcClient()

    k1 = bytes(b"K1...................")
    v1 = bytes(b"V1...................")

    k2 = bytes(b"K2...................")
    v2 = bytes(b"V2...................")

    write_result = client.write(msg=raw_write_req(
        readset=[],
        writeset=[(k1, v1), (k2, v2)],
        block_id=0)
    )

    r = parse_write_reply(write_result)
    assert r.success
    print("SKVBC write: OK")

    result_kvs = parse_read_reply(
        client.read(msg=raw_read_req(readset=[k1])))

    assert result_kvs[k1] == v1
    print("SKVBC read: OK")


if __name__ == "__main__":
    test_skvbc_client()
