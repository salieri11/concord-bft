#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import json
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.orchestration_pb2 as orchestration
import vmware.blockchain.deployment.v1.provisioning_service_pb2 as provision_service
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
    parser.add_argument("--network-gateway", type=int, help="VM network gateway address")
    parser.add_argument("--network-subnet", type=int, help="VM network subnet size")
    parser.add_argument(
        "--resource-pool",
        default="Compute-ResourcePool",
        help="Resource pool to utilize for every SDDC"
    )
    parser.add_argument(
        "--container-registry",
        default="https://registry-1.docker.io/v2",
        help="Container registry to use for an orchestration site"
    )
    parser.add_argument(
        "--container-registry-username",
        default="blockchainrepositoryreader",
        help="Container registry user login"
    )
    parser.add_argument("--container-registry-password", help="Container registry user password")

    return vars(parser.parse_args())


def to_signed_int(value: int, bits: int = 64) -> int:
    """
    Interpret an integer input as an unsigned value of specified bit precision, and return the
    byte-content equivalent signed value.

    Args:
        value (int): input value.
        bits (int): bit precision to use to interpret the input value as unsigned value.
    
    Returns:
        int: value reinterpreted as signed integer of specified bit precision.
    """
    mask = (1 << bits) - 1

    return (value | ~mask) if value & (1 << (bits - 1)) else (value & mask)


def new_orchestration_site(
        organization: str,
        data_center: str,
        api_token: str,
        folder: str,
        resource_pool: str,
        network: str,
        network_gateway: int,
        network_subnet: int,
        container_registry: str,
        container_registry_username: str,
        container_registry_password: str
) -> orchestration.OrchestrationSite:
    """
    Create a new OrchestrationSite instance based on supplied parameters.

    Args:
        organization (str): VMware Cloud organization account ID.
        data_center (str): SDDC data center ID.
        api_token (str): API token used for connecting to VMC.
        folder (str): folder within SDDC data center to use for VM deployment.
        resource_pool (str): resource pool to utilize for VM deployment.
        network (str): VM network to utilize for VM deployment.
        network_gateway (int): network CIDR gateway for VM network, if VM network does not exist.
        network_subnet (int): network subnet size for VM network, if VM network does not exist.
        container_registry (str): container registry to use to obtain model images.
        container_registry_username (str): container registry user login.
        container_registry_password (str): container registry user password.

    Returns:
        orchestration.OrchestrationSite: a new instance of the corresponding orchestration site.
    """
    site_uuid = uuid.UUID(data_center)
    site_id = orchestration.OrchestrationSiteIdentifier(
        id=site_uuid
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
                    address=container_registry,
                    credential=core.Credential(
                        type=core.Credential.PASSWORD,
                        password_credential=core.PasswordCredential(
                            username=container_registry_username,
                            password=container_registry_password
                        )
                    )
                ),
                organization=organization,
                datacenter=data_center,
                resource_pool=resource_pool,
                folder=folder,
                control_network=network,
                control_network_gateway=network_gateway,
                control_network_subnet=network_subnet
            )
        )
    )


def main():
    """
    Main program entry-point.

    Example:
        $ python vmc_generate_config.py
            --organization=c56e116e-c36f-4f7d-b504-f9a33955b853
            --data-centers a890ac97-941d-4479-a90c-98061c1e3639 3656526b-c74e-4f87-8e1f-a667975273c2
            --api-token=00000000-0000-0000-0000-000000000000
            --folder=Workloads
            --network=vmware-vpn
            --network-gateway=172518401
            --network-subnet=24
            --container-registry=https://registry-1.docker.io/v2,
            --container-registry-username=blockchainrepositoryreader
            --container-registry-password=some_password

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
                network_gateway=args["network_gateway"],
                network_subnet=args["network_subnet"],
                container_registry=args["container_registry"],
                container_registry_username=args["container_registry_username"],
                container_registry_password=args["container_registry_password"]
            ) for site in args["data_centers"]
        ]
    )

    # Generate JSON according to Protocol Buffer's JSON codec.
    config_json = json.loads(MessageToJson(config))

    # Dump JSON to STDOUT.
    print(json.dumps(config_json, indent=4))


if __name__ == "__main__":
    main()
