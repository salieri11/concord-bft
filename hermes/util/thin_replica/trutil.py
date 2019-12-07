#!/usr/bin/env python3

# Manual testing via grpcurl, e.g.:
#
# $> grpcurl -plaintext -d @ localhost:50051 com.vmware.concord.thin_replica.ThinReplica.ReadStateHash <<EOM
# { "block_id" : 1337 }
# EOM

import grpc
import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import thin_replica_pb2_grpc as trgrpc
import thin_replica_pb2 as trproto

class ThinReplica:
    def __init__(self, host, port):
        self.channel = grpc.insecure_channel("{}:{}".format(host, port))
        self.stub = trgrpc.ThinReplicaStub(self.channel)

    def __del__(self):
        self.channel.close()

    def read_state(self):
        request = trproto.ReadStateRequest()
        return self.stub.ReadState(request)

    def read_hash(self, block_id):
        request = trproto.ReadStateHashRequest(block_id=block_id)
        return self.stub.ReadStateHash(request)
