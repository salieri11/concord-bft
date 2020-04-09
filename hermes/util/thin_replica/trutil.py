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
from google.protobuf import empty_pb2 as google_dot_protobuf_dot_empty__pb2

class ThinReplica:
    def __init__(self, host="localhost", port="50051",
                       client_id=os.path.basename(__file__)):
        self.channel = grpc.insecure_channel("{}:{}".format(host, port))
        self.stub = trgrpc.ThinReplicaStub(self.channel)
        self.meta = [("client_id", client_id)]

    def __del__(self):
        self.channel.close()

    def read_state(self, key_prefix=b""):
        request = trproto.ReadStateRequest(key_prefix=key_prefix)
        return self.stub.ReadState(request, metadata=self.meta)

    def read_hash(self, block_id, key_prefix=b""):
        request = trproto.ReadStateHashRequest(block_id=block_id, key_prefix=key_prefix)
        return self.stub.ReadStateHash(request, metadata=self.meta)

    def subscribe_to_updates(self, block_id=1, key_prefix=b""):
        request = trproto.SubscriptionRequest(block_id=block_id, key_prefix=key_prefix)
        return self.stub.SubscribeToUpdates(request, metadata=self.meta)

    def subscribe_to_update_hashes(self, block_id=1, key_prefix=b""):
        request = trproto.SubscriptionRequest(block_id=block_id, key_prefix=key_prefix)
        return self.stub.SubscribeToUpdateHashes(request, metadata=self.meta)

    def unsubscribe(self):
        request = google_dot_protobuf_dot_empty__pb2.Empty()
        return self.stub.Unsubscribe(request)
