
import json
import requests
from collections import namedtuple
from lib import utils, vsphere, ipam_utils
from config import common

class VmcSddc():
    """
        Class defining interaction with VMC
    """
    def __init__(self, name):
        self.name = name
        self.vmc_data = utils.get_vault_constants("VMC")
        self.dc_data = self.vmc_data["SDDCS"][name]
        self.auth_header = utils.get_auth_header(common.CSP_PROD,
                                   self.vmc_data["CSP_API_TOKEN"])
        self.logger = utils.setup_logging()
        self.vc = vsphere.Vsphere(hostname=self.dc_data['vcenter'],
                                username=self.dc_data['vc_user'],
                                password=self.dc_data['vc_pwd'])


    def get_network_segment(self, name):
        """
            Get segment data given name
        """
        url = ("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                    "policy/api/v1/infra/tier-1s/cgw/segments/%s" %
                    (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                    self.dc_data['id'], name))
        try:
            return utils.parse_request_json(url, header_dict=self.auth_header)
        except Exception as e:
            self.logger.exception("Error getting network segment data %s %s" %
                                (name, e))
            return False


    def get_public_ips(self):
        """
            Get all public ips from vmc nsx mgr
        """
        url = ('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
                'cloud-service/api/v1/infra/public-ips/' %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                 self.dc_data["id"]))
        try:
            return utils.parse_request_json(url, header_dict=self.auth_header)
        except Exception as e:
            self.logger.error("Error getting public ips for sddc %s %s: %s" %
                        (sddc, req.text, e))
            return False


    def get_public_ip(self, publicip):
        """
            Get given public ip from vmc nsx mgr
        """
        url = ('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
                'cloud-service/api/v1/infra/public-ips/%s' %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                 self.dc_data["id"], publicip))
        try:
            return utils.parse_request_json(url, header_dict=self.auth_header)
        except Exception as e:
            self.logger.exception("Error getting pubic ip %s %s" %
                                (publicip, e))
            return False


    def delete_public_ip(self, publicip):
        """
            Delete public ip from vmc sddc given public ip
        """
        url = ('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
                'cloud-service/api/v1/infra/public-ips/%s' %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                 self.dc_data["id"], publicip))
        rc, rv = utils.request_url(url, header_dict=self.auth_header,
                                    method="DELETE")
        if rc == 200:
            self.logger.error("Successfully deleted publicip %s" % publicip)
        else:
            self.logger.error("Error deleting publicip %s %s" % (publicip, rv))
            return False


    def get_nat_rule(self, natrule):
        """
            Get nat rule for vmc sddc
        """
        url = ("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                "policy/api/v1/infra/tier-1s/cgw/nat/USER/nat-rules/%s" %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                 self.dc_data["id"], natrule))
        try:
            return utils.parse_request_json(url, header_dict=self.auth_header)
        except Exception as e:
            self.logger.exception("Error getting nat rule %s %s" % (natrule,e))
            return False

    def get_nat_rules(self):
        """
            Get nat rule for sddc
        """
        url = ("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                "policy/api/v1/infra/tier-1s/cgw/nat/USER/nat-rules" %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                self.dc_data["id"]))
        try:
            return utils.parse_request_json(url, header_dict=self.auth_header)
        except Exception as e:
            logger.info("Error getting nat rules for sddc %s" % e)
            return False


    def delete_nat_rule(self, natrule):
        """
            Delete nat rule
        """
        url = ("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                "policy/api/v1/infra/tier-1s/cgw/nat/USER/nat-rules/%s" %
                (self.dc_data["nsx_mgr"], self.vmc_data["ORG_ID"],
                self.dc_data["id"], natrule))
        rc, rv = utils.request_url(url, header_dict=self.auth_header,
                                    method="DELETE")
        if rc == 200:
            self.logger.error("Successfully deleted nat rule %s" % natrule)
        else:
            self.logger.error("Error deleting nat rule %s %s" % (natrule, rv))
            return False


    def filter_deployment_rules(self):
        """
            Filter persephone deployment specific nat rules and eip,
            which typically are reflexive rules and following uuid name
            pattern
        """
        all_natrules = self.get_nat_rules()
        deployment_rules = list(filter(lambda rule: rule['action'] == 'REFLEXIVE'
                    and utils.validate_uuid4(rule["display_name"]) is True,
                    all_natrules["results"]))
        nwinfo =  [
                    {'id': rule['display_name'],
                    'internal_ip': rule['source_network'],
                    'external_ip': rule['translated_network']}
                    for rule in deployment_rules
                  ]
        return nwinfo

    def delete_ipam_resources(self, vms):
        """
            Retrieve cidr and ipaddr of vm to delete ipam entry
        """
        metadata = utils.get_network_metadata([vm.name for vm in vms])
        for vm in vms:
            natid = metadata[vm.name]
            data = self.get_nat_rule(natid)
            if data is False:
                self.logger.info("Nat rule does not exist, \
                                checking for guest ip")
                if vm.runtime.powerState == "poweredOn":
                    ipaddr = vm.guest.ipAddress
                else:
                    self.logger.info("Vm not powered on, skipping ipam cleanup")
                    continue
            else:
                ipaddr = data["source_network"]
            self.logger.info("Deleting ipam for %s with address %s" %
                        (vm.name, ipaddr))
            ipaddr_hex = ipam_utils.nwaddress_to_hex(ipaddr)
            nw_segment = vm.network[0].name
            nw_segment_data = self.get_network_segment(nw_segment)
            cidr_hex = ipam_utils.nwaddress_to_hex(
                                        nw_segment_data['subnets'][0]['network'])
            ipam_utils.delete_ipam_entry(nw_segment, ipaddr_hex, cidr_hex,
                                        self.dc_data["id"])

