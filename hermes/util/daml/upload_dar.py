#!/usr/bin/env python3

import argparse
import grpc
import logging
import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import package_management_service_pb2_grpc as daml_package_grpc
import package_management_service_pb2 as daml_package_pb

def upload_dar(host, port, darfile):
   '''
   TODO: Consider using the --timeout parameter.  Would need to regenerate
   bindings probably; I don't see the timeout parameter in the definition
   of UplodDarFileRequest().
   Or use the DAML SDK.
   '''
   with open(darfile, 'rb') as dar:
      darRequest = daml_package_pb.UploadDarFileRequest(dar_file=dar.read())

   response = False
   with grpc.insecure_channel("{}:{}".format(host, port)) as channel:
      stub = daml_package_grpc.PackageManagementServiceStub(channel)
      response = stub.UploadDarFile(darRequest)
   return response

def argparser(args):
   parser = argparse.ArgumentParser()
   parser.add_argument("--ledger", default="localhost:6861")
   parser.add_argument("--dar", required=True)
   return parser

def main(argv):
   args = argparser(argv).parse_args()
   (host, port) = args.ledger.split(":")

   assert os.path.exists(args.dar)
   if not upload_dar(host=host, port=port, darfile=args.dar):
      logging.error("Failed to upload")

if __name__ == "__main__":
   main(sys.argv[1:])
