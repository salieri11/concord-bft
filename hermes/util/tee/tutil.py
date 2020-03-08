#!/usr/bin/env python3

# Manual testing via grpcurl, e.g.:
#
# $ grpcurl -plaintext -d '{ "test_input" : "x" }' 127.0.0.1:50051 
# com.vmware.concord.tee.TeeService/RunTest
#
import grpc
import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import tee_pb2_grpc as tgrpc
import tee_pb2 as tproto

class Tee:
    def __init__(self, host="localhost", port="50051",
                       client_id=""):
        self.channel = grpc.insecure_channel("{}:{}".format(host, port))
        self.stub = tgrpc.TeeServiceStub(self.channel)
        self.meta = [("client_id", client_id)]

    def __del__(self):
        self.channel.close()

    def run_test(self, test_input=b""):
        request = tproto.TestInput(test_input=test_input)
        return self.stub.RunTest(request, metadata=self.meta)

    def skvbc_write(self, raw_skvbc_write):
        raw_skvbc_write = tproto.RawSkvbcRequest(content=bytes(raw_skvbc_write))
        result = self.stub.SkvbcWrite(raw_skvbc_write, metadata=self.meta)

        return result.content

    def skvbc_read(self, raw_skvbc_read):
        raw_skvbc_read = tproto.RawSkvbcRequest(content=bytes(raw_skvbc_read))
        result = self.stub.SkvbcRead(raw_skvbc_read, metadata=self.meta)

        return result.content
