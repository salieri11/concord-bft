#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import persephone.core_pb2 as core
import persephone.orchestration_pb2 as orchestration
import persephone.provisioning_service_pb2 as provision_service
import uuid
from google.protobuf.json_format import MessageToJson
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        Dict[str, Any]: mapping containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone Metadata Service Client")
    parser.add_argument("--api-token", help="VMware Cloud Service Platform API token")
    parser.add_argument("--organization", help="VMware Cloud organization account ID")
    parser.add_argument("--data-centers", nargs='*', help="List of SDDC IDs")
    parser.add_argument("--folder", help="VM folder within each SDDC to place VMs under")
    parser.add_argument("--network", help="VM network to utilize for every SDDC")
    parser.add_argument("--network-prefix", type=int, help="VM network prefix address")
    parser.add_argument("--network-subnet", type=int, help="VM network subnet size")
    parser.add_argument(
        "--resource-pool",
        default="Compute-ResourcePool",
        help="Resource pool to utilize for every SDDC"
    )

    return vars(parser.parse_args())


def new_orchestration_site(
        organization: str,
        data_center: str,
        api_token: str,
        folder: str,
        resource_pool: str,
        network: str,
        network_prefix: int,
        network_subnet: int
) -> orchestration.OrchestrationSite:
    """

    Args:
        organization (str): VMware Cloud organization account ID.
        data_center (str): SDDC data center ID.
        api_token (str): API token used for connecting to VMC.
        folder (str): Folder within SDDC data center to use for VM deployment.
        resource_pool (str): Resource pool to utilize for VM deployment.
        network (str): VM network to utilize for VM deployment.
        network_prefix (int): Network CIDR prefix for VM network, if VM network does not exist.
        network_subnet (int): Network subnet size for VM network, if VM network does not exist.

    Returns:
        orchestration.OrchestrationSite: a new instance of the corresponding orchestration site.
    """
    site_uuid = uuid.UUID(data_center)
    site_id = orchestration.OrchestrationSiteIdentifier(
        low=(site_uuid.int >> 64),
        high=(site_uuid.int & 0xFFFFFFFFFFFFFFFF)
    )
    return orchestration.OrchestrationSite(
        id=site_id,
        info=orchestration.OrchestrationSiteInfo(
            type=orchestration.OrchestrationSiteInfo.VMC,
            vmc=orchestration.VmcOrchestrationSiteInfo(
                authentication=core.Endpoint(
                    address="https://console.cloud.vmware.com",
                    credential=core.Credential(
                        type=core.Credential.BEARER,
                        token_credential=core.BearerTokenCredential(token=api_token)
                    )
                ),
                api=core.Endpoint(address="https://vmc.vmware.com"),
                container_registry=core.Endpoint(
                    address="https://registry-1.docker.io/v2",
                    credential=core.Credential(
                        type=core.Credential.PASSWORD,
                        password_credential=core.PasswordCredential(
                            username="blockchainrepositoryreader",
                            password="j4jshdh$@ED2R$*Trf8"
                        )
                    )
                ),
                organization=organization,
                datacenter=data_center,
                resource_pool=resource_pool,
                folder=folder,
                control_network=network,
                control_network_prefix=network_prefix,
                control_network_subnet=network_subnet
            )
        )
    )


def main():
    """
    Main program entry-point.

    Example:
        $ python vmc_generate_config.py
            --org=c56e116e-c36f-4f7d-b504-f9a33955b853
            --data-centers a890ac97-941d-4479-a90c-98061c1e3639 3656526b-c74e-4f87-8e1f-a667975273c2
            --api-token=572c696a-298c-4e73-bc7d-868a3179f70a
            --folder=Workloads
            --network=vmware-vpn
            --network-prefix=172518400
            --network-subnet=24

    Returns:
        None
    """
    args = parse_arguments()
    config = provision_service.ProvisioningServerConfiguration(
        port=9002,
        transport_security=core.TransportSecurity(
            type=core.TransportSecurity.NONE
        ),
        sites=[
            new_orchestration_site(
                organization=args["organization"],
                data_center=site,
                api_token=args["api_token"],
                folder=args["folder"],
                resource_pool=args["resource_pool"],
                network=args["network"],
                network_prefix=args["network_prefix"],
                network_subnet=args["network_subnet"]
            ) for site in args["data_centers"]
        ]
    )
    print(MessageToJson(config))


if __name__ == "__main__":
    main()
