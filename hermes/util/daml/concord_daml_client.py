#!/usr/bin/env python3

# The client can be tested by running the following commands:
# - start a DAML nano deployment
# - from within hermes/util/daml, and a Hermes virtual environment, run:
#    python3 concord_daml_client.py

import grpc
import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import daml_commit_pb2 as daml_proto
import daml_commit_pb2_grpc as daml_grpc


class ConcordDamlClient:
    """
    This class is a client to the Concord node's DAML gRPC API.
    For example, it provides an easy way to send commit transaction
    messages directly to Concord (without going through the Ledger API node).

    Another great benefit it provides is the ability to control the
    transaction processing flags. This allows us for example to trigger the
    pre-execution flow on a given transaction (by passing flags=0x2).
    """

    def __init__(self, host="localhost", port="50051",
                 client_id=""):
        self._channel = grpc.insecure_channel("{}:{}".format(host, port))
        self._stub = daml_grpc.CommitServiceStub(self._channel)
        self._meta = [("client_id", client_id)]

    def __del__(self):
        self._channel.close()

    def commit_transaction(self,
           submission,
           correlation_id="cid",
           participant_id="default_participant",
           flags=0):

        request = daml_proto.CommitRequest(
            submission=submission,
            correlation_id=correlation_id,
            participant_id=participant_id,
            flags=flags
        )
        return self._stub.CommitTransaction(request, metadata=self._meta)


if __name__ == "__main__":
    client = ConcordDamlClient()

    # the below submission is the binary representation of creating a party, as in:
    # daml ledger allocate-parties --host localhost --port 6861 PartyName
    submission = bytes.fromhex(
        "08041801226A1F8B0800000000000000E3B2B2B21052"
        "B134324C4B32494AD3B530B448D63549B2B0D4B54C4D"
        "33D34D314F314F4A36374D4C34B090124849CCCD89CF"
        "494D494F2D8A4F2CC834E4E256E20C482C2AA9F44BCC"
        "4DD5F2E322CA182184164C1391CC0300427431ED9900"
        "0000")

    client.commit_transaction(submission)
