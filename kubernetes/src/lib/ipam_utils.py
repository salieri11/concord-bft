import ipaddress
import json
import os
import tempfile
from config import common
from lib import utils

logger = utils.setup_logging()

def nwaddress_to_hex(address):
    """
        Convert address to hex
        if cidr return first address on the block
    """
    if address.find("/") == -1:
        return '{:02x}{:02x}{:02x}{:02x}'.format(
                *map(int, address.split(".")))
    else:
        network = ipaddress.IPv4Network(address)
        first_address = network[0]
        return '{:02x}{:02x}{:02x}{:02x}'.format(
                *map(int, first_address.exploded.split(".")))


def source_ipam_certificate():
    """
    """
    ipam_data = utils.get_vault_constants("ipam", logger=logger)
    server_crt_file = None
    with tempfile.NamedTemporaryFile(delete=False) as server_crt:
        server_crt.write(ipam_data["server_crt"].encode())
        server_crt_file = server_crt.name
    return server_crt_file


def delete_ipam_entry(nw_name, ipaddr, cidr, sddc_id, onprem=False):
    """
        Delete ipam entry with grpc_curl
    """
    cert_path = source_ipam_certificate()
    if onprem is False:
        data = {"name":
                    "blocks/%s-%s/segments/%s/addresses/%s" % (
                    sddc_id, nw_name, cidr, ipaddr)}
    else:
        data = {"name":
                    "blocks/%s/segments/%s/addresses/%s" % (
                    nw_name, cidr, ipaddr)}
    release_method = ("vmware.blockchain.deployment."
                       "v1.IPAllocationService.ReleaseAddress")
    cmd =  ("grpcurl -format=json -d='%s' -cacert=%s %s %s" %
                            (json.dumps(data), cert_path,
                            common.IPAM_URL, release_method))
    rc, rv = utils.subproc(cmd, logger=logger, timeout=10)
    os.remove(cert_path)
    if rc == 0:
        logger.info("Successfully removed ipam entry %s" % ipaddr)
        return True
    else:
        logger.error("Error cleaning up ipam entry %s:%s" % (ipaddr, rv))
        return False
