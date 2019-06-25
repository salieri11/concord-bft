#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import persephone.core_pb2 as core
import persephone.ip_allocation_service_pb2 as ip_allocation_service
import persephone.ip_allocation_service_pb2_grpc as ip_allocation_service_rpc
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="IP Allocation Service Client")
    parser.add_argument(
        "--server",
        default="localhost:9099",
        help="Service endpoint (default: localhost:9099)"
    )
    parser.add_argument(
        "--trusted-certs",
        default=None,
        help="File path to trusted server certificates"
    )
    return vars(parser.parse_args())


def main():
    """
    Main program entry-point.

    Example:
        $ python ipam_client.py --server localhost:9099 --trusted-certs /tmp/server.crt

    Returns:
        None
    """
    args = parse_arguments()
    if args["trusted_certs"]:
        with io.open(args["trusted_certs"], "rb") as f:
            trusted_certs = f.read()
        credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
        channel = grpc.secure_channel(args["server"], credentials)
    else:
        channel = grpc.insecure_channel(args["server"])
    stub = ip_allocation_service_rpc.IPAllocationServiceStub(channel)

    prefix = 0x0A010000
    subnet = 24
    create_address_block_request = ip_allocation_service.CreateAddressBlockRequest(
        header=core.MessageHeader(),
        block_id="test-block",
        block=ip_allocation_service.AddressBlockSpecification(
            prefix=prefix,
            subnet=subnet
        ),
        reserved_allocations={
            prefix: bytes([
                0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80
            ])
        }
    )
    create_address_block_response = stub.CreateAddressBlock(create_address_block_request)
    print("CreateAddressBlock(): ", create_address_block_response)

    for i in range(1 << (32 - subnet)):
        try:
            allocate_request = ip_allocation_service.AllocateAddressRequest(
                header=core.MessageHeader(),
                parent=create_address_block_response.name,
            )
            allocate_response = stub.AllocateAddress(allocate_request)
            print("AllocateAddress(): ", allocate_response)
        except Exception as error:
            print(error)

    delete_address_block_request = ip_allocation_service.DeleteAddressBlockRequest(
        header=core.MessageHeader(),
        name=create_address_block_response.name
    )
    delete_address_block_response = stub.DeleteAddressBlock(delete_address_block_request)
    print("DeleteAddressBlock(): ", delete_address_block_response)


if __name__ == "__main__":
    main()
