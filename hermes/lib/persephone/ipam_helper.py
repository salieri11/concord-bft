#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import collections
import grpc
import io
import logging
import util.hermes_logging
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.ip_allocation_service_pb2 as ip_allocation_service
import vmware.blockchain.deployment.v1.ip_allocation_service_pb2_grpc as ip_allocation_service_rpc
from typing import Any, Dict, List

log = util.hermes_logging.getMainLogger()

CONNECTION = {
  "server": "ipam-vmbc.cloud.vmware.com",
  "certFile": "../docker/config-persephone/persephone/provisioning/ipam.crt",
  "stub": None,
}

def prepare_stub():
  if CONNECTION["stub"]: return CONNECTION["stub"]
  with io.open(CONNECTION["certFile"], "rb") as f:
      trusted_certs = f.read()
  credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
  channel = grpc.secure_channel(CONNECTION["server"], credentials)
  stub = ip_allocation_service_rpc.IPAllocationServiceStub(channel)
  CONNECTION["stub"] = stub
  log.debug("Successfully connected to IPAM on {}".format(CONNECTION["server"]))
  return stub


def create_block(network_name, block_name, block_prefix, subnet_range, reserved_list, dryRun=False):
  stub = prepare_stub()
  if not dryRun: create_address_block(stub, block_name, block_prefix, subnet_range, reserved_list)
  else: log.info("[{}] Would have created network segment with (block: {}, prefix: {}, range: {}, reserved: {})".format(
                 network_name, block_name, block_prefix, subnet_range, ", ".join(reserved_list)))


def remove_block(network_name, block_name, dryRun=False):
  stub = prepare_stub()
  if not dryRun: delete_address_block(stub, block_name)
  else: log.info("[{}] Would have removed this network segment block: {}".format(
                 network_name, block_name))


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

