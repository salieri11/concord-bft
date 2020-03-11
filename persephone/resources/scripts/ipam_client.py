#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import collections
import grpc
import io
import logging
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.ip_allocation_service_pb2 as ip_allocation_service
import vmware.blockchain.deployment.v1.ip_allocation_service_pb2_grpc as ip_allocation_service_rpc
from typing import Any, Dict, List


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
        "--command",
        default=None,
        choices=["add", "remove", "test"],
        help="Command option"
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
        type=int,
        default=24,
        help="Subnet prefix for the block (default: 24)"
    )
    parser.add_argument(
        "--reserved",
        action="append",
        help="Reserved IP address or address range"
    )
    return vars(parser.parse_args())


def default_block_reservation() -> bytearray:
    """
    Default byte-array representing an unallocated bitmap for a 256-IP-addresses block segment.

    Returns:
        a new zero-ed out byte-array of 32 bytes.
    """
    return bytearray([
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ])


def to_block_reservation(reserved: List[str], prefix: int, broadcast: int) -> Dict[int, bytes]:
    """
    Convert a given list of strings of IP or IP ranges, a block prefix address, and a broadcast
    address, and create a dictionary of 256-address block segment prefix address to 32-byte byte-
    array representing the reserved allocations.

    Args:
        reserved (List[str]): List of IP (hex) or IP ranges (hex-hex) denoting reserved addresses.
        prefix (int): block prefix address.
        broadcast (int): block broadcast address.

    Returns:
        a dictionary of block segment prefix to its 32-byte-wide reserved addresses byte-array.
    """
    log = logging.getLogger("main")

    reserved_allocations = collections.defaultdict(default_block_reservation)
    for str_value in reserved:
        value = [int("0x" + x, 16) for x in str_value.split("-", maxsplit=1)]
        ip_range = range(value[0], value[-1] + 1)
        for ip in ip_range:
            if prefix <= ip <= broadcast:
                # Find the 256-address block prefix address and offset within the block.
                ip_block_offset = ip % (1 << 8)
                ip_block_byte_offset = ip_block_offset >> 3
                ip_block_bit_offset = ip_block_offset % 8
                ip_block_prefix = ip - ip_block_offset

                log.info(
                    "Reserve %x: prefix(%x), offset(%d), byte(%d/%d)",
                    ip, ip_block_prefix, ip_block_offset, ip_block_byte_offset, ip_block_bit_offset
                )

                byte_value = reserved_allocations[ip_block_prefix][ip_block_byte_offset]
                byte_value |= 2**ip_block_bit_offset
                reserved_allocations[ip_block_prefix][ip_block_byte_offset] = byte_value

    return {k: bytes(v) for k, v in reserved_allocations.items()}


def create_address_block(
        stub: ip_allocation_service_rpc.IPAllocationServiceStub,
        block_name: str,
        block_prefix: str,
        subnet: int,
        reserved: List[str] = []
) -> ip_allocation_service.CreateAddressBlockResponse:
    """
    Create an address block using supplied parameters.

    Args:
        stub (ip_allocation_service_rpc.IPAllocationServiceStub): Service stub.
        block_name (str): Name of the address block to create.
        block_prefix (str): Starting id of the block
        subnet (int): Subnet size (e.g. 22, 24, 28, 30).
        reserved (list[str]): List of addressed reserved from allocation.

    Returns:
        address block creation response from server.
    """
    log = logging.getLogger("main")

    prefix = int("0x" + block_prefix, 16)
    broadcast = prefix + (1 << (32 - subnet)) - 1
    reserved_allocations = to_block_reservation(reserved, prefix, broadcast)
    for key, value in reserved_allocations.items():
        for i in enumerate(value):
            log.info("Reserved Block %x: %s", key, i)

    create_address_block_request = ip_allocation_service.CreateAddressBlockRequest(
        header=core.MessageHeader(),
        block_id=block_name,
        block=ip_allocation_service.AddressBlockSpecification(
            prefix=prefix,
            subnet=subnet
        ),
        reserved_allocations=reserved_allocations
    )
    create_address_block_response = stub.CreateAddressBlock(create_address_block_request)
    log.info("CreateAddressBlock(): %s", create_address_block_response)

    return create_address_block_response


def delete_address_block(
        stub: ip_allocation_service_rpc.IPAllocationServiceStub,
        block_name: str
) -> ip_allocation_service.DeleteAddressBlockResponse:
    """
    Deletes a given address block.

    Args:
        stub: (ip_allocation_service_rpc.IPAllocationServiceStub): Service stub.
        block_name: Name of the address block to delete.

    Returns:
        address block deletion response from server.
    """
    log = logging.getLogger("main")

    block_name = "blocks/" + block_name
    delete_address_block_request = ip_allocation_service.DeleteAddressBlockRequest(
        header=core.MessageHeader(),
        name=block_name
    )
    delete_address_block_response = stub.DeleteAddressBlock(delete_address_block_request)
    log.info("DeleteAddressBlock(): %s", delete_address_block_response)

    return delete_address_block_response


def smoke_test(
        stub: ip_allocation_service_rpc.IPAllocationServiceStub,
        block_name: str,
        block_prefix: str,
        subnet: int
):
    """
    Performs a smoke functionality test against a service endpoint.

    Args:
        stub (ip_allocation_service_rpc.IPAllocationServiceStub): Service stub.
        block_name (str): Name of the address block to create.
        block_prefix (str): Starting id of the block
        subnet (int): Subnet size (e.g. 22, 24, 28, 30).

    Returns:
        None
    """
    log = logging.getLogger("main")

    create_address_block_response = create_address_block(
        stub,
        block_name,
        block_prefix,
        subnet
    )
    for i in range(1 << (32 - subnet)):
        try:
            allocate_request = ip_allocation_service.AllocateAddressRequest(
                header=core.MessageHeader(),
                parent=create_address_block_response.name,
            )
            allocate_response = stub.AllocateAddress(allocate_request)
            log.info("AllocateAddress(): %s", allocate_response)
        except Exception as error:
            log.error(error)
    delete_address_block(stub, block_name)
    return


def main():
    """
    Main program entry-point.

    Example:
        $ python ipam_client.py --server localhost:9099
            [--trusted-certs /tmp/server.crt]
            [--ca-cert true]
            --option test/add/remove
            --block-name temp
            --block-prefix 0a0b0c00
            [--subnet 24]
            [--reserved 0a0b0c00-0a0b0c01]
            [--reserved 0a0b0cff]

    Returns:
        None
    """
    # Setup logging.
    logging.basicConfig(
        level=logging.DEBUG,
        format="[%(asctime)s] [%(levelname)s] [%(name)s]: %(message)s",
    )

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

    command = args["command"]
    if command == "test":
        smoke_test(stub, block_name, block_prefix, subnet_range)
    elif command == "add":
        create_address_block(stub, block_name, block_prefix, subnet_range, args["reserved"])
    elif command == "remove":
        delete_address_block(stub, block_name)


if __name__ == "__main__":
    main()
