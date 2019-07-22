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
    parser.add_argument(
        "--option",
        default=None,
        help="Valid Values: test/add/remove"
    )
    parser.add_argument(
        "--block-name",
        default=None,
        help="Sddc-id hyphen segment name"
    )
    parser.add_argument(
        "--block-prefix",
        default=None,
        help="ID of the block. ex: 0a0b0300"
    )
    parser.add_argument(
        "--subnet",
        default=24,
        help="Subnet prefix for the block. Default is 24"
    )
    return vars(parser.parse_args())


def create_address_block(stub, block_name, block_prefix, subnet) -> ip_allocation_service.CreateAddressBlockResponse:
    """
    Create an address block using supplied parameters.

    Args:
        stub (ip_allocation_service_rpc.IPAllocationServiceStub): Service stub.
        block_name (str): Name of the address block to create.
        block_prefix (str): Starting id of the block
        subnet (int): Subnet size (e.g. 22, 24, 28, 30).
    Returns:
        address block creation response from server.
    """

    prefix = int("0x" + block_prefix, 16)
    create_address_block_request = ip_allocation_service.CreateAddressBlockRequest(
        header=core.MessageHeader(),
        block_id=block_name,
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
    return create_address_block_response


def delete_address_block(stub, block_name) -> ip_allocation_service.DeleteAddressBlockResponse:
    """
    Deletes a given address block.
    Args:
        stub: (ip_allocation_service_rpc.IPAllocationServiceStub): Service stub.
        block_name: Name of the address block to delete.
    Returns:
        address block deletion response from server.
    """
    block_name = "blocks/" + block_name
    delete_address_block_request = ip_allocation_service.DeleteAddressBlockRequest(
        header=core.MessageHeader(),
        name=block_name
    )
    delete_address_block_response = stub.DeleteAddressBlock(delete_address_block_request)
    print("DeleteAddressBlock(): ", delete_address_block_response)
    delete_address_block_response


def test_ipam(stub, block_name, block_prefix, subnet_range):

    create_address_block_response = create_address_block(stub, block_name, block_prefix, subnet_range)
    for i in range(1 << (32 - subnet_range)):
        try:
            allocate_request = ip_allocation_service.AllocateAddressRequest(
                header=core.MessageHeader(),
                parent=create_address_block_response.name,
            )
            allocate_response = stub.AllocateAddress(allocate_request)
            print("AllocateAddress(): ", allocate_response)
        except Exception as error:
            print(error)
    delete_address_block(stub, block_name)


def main():
    """
    Main program entry-point.

    Example:
        $ python ipam_client.py --server localhost:9099 [--trusted-certs /tmp/server.crt] [--ca-cert true]
        --option test/add/remove --block-name temp --block-id 0a0b0c00 [--subnet 24]

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

    block_name = args["block_name"]
    block_prefix = args["block_prefix"]
    subnet_range = args["subnet"]

    if args["option"] == "test":
        test_ipam(stub, block_name, block_prefix, subnet_range)
    elif args["option"] == "add":
        create_address_block(stub, block_name, block_prefix, subnet_range)
    elif args["option"] == "remove":
        delete_address_block(stub, block_name)
    else:
        print("unsupported args...")


if __name__ == "__main__":
    main()
