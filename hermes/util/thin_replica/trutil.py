#!/usr/bin/env python3

# Manual testing via grpcurl, e.g.:
#
# $> grpcurl -plaintext -d @ localhost:50051 com.vmware.concord.thin_replica.ThinReplica.ReadStateHash <<EOM
# { "block_id" : 1337 }
# EOM
import logging
import grpc
import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import thin_replica_pb2_grpc as trgrpc
import thin_replica_pb2 as trproto
from google.protobuf import empty_pb2 as google_dot_protobuf_dot_empty__pb2

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

class ThinReplica:
    def __init__(self, host="localhost", port="50051", host_name="concord1", use_tls=False,
                       client_id=os.path.basename(__file__)):
        if (use_tls):
            # verify the thin replica server is also using TLS
            log.info("TLS enabled for the trutil client")

            # for simplicity, the trutil client uses the same cert as daml_ledger_api1
            with open("../docker/trs_trc_tls_certs/s{}/server.cert".format(int(host_name[-1]) - 1), 'rb') as root,\
                 open("../docker/trs_trc_tls_certs/c0/client.cert", 'rb') as cert,\
                 open("../docker/trs_trc_tls_certs/c0/pk.pem", 'rb') as key:
                creds = grpc.ssl_channel_credentials(root.read(), key.read(), cert.read())
            # override ssl target name with the host_name, as certificates are generated
            # with CN=hostname
            log.info("Successfully read all the certificates")
            self.channel = grpc.secure_channel("{}:{}".format(host, port), creds,
                                                options=[('grpc.ssl_target_name_override',
                                                host_name)])
        else:
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
