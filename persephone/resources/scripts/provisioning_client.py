#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import persephone.core_pb2 as core
import persephone.concord_model_pb2 as concord_model
import persephone.ethereum_pb2 as ethereum
import persephone.orchestration_service_pb2 as orchestration_service
import persephone.orchestration_service_pb2_grpc as orchestration_service_rpc
import persephone.provisioning_service_pb2 as provisioning_service
import persephone.provisioning_service_pb2_grpc as provisioning_service_rpc
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone Provisioning Service Client")
    parser.add_argument(
        "--server",
        default="localhost:9002",
        help="Service endpoint (default: localhost:9002)"
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
        $ python provisioning_client.py --server localhost:9002 --trusted-certs /tmp/server.crt

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
    provisioning_stub = provisioning_service_rpc.ProvisioningServiceStub(channel)
    orchestration_site_stub = orchestration_service_rpc.OrchestrationSiteServiceStub(channel)

    orchestration_site_list_request = orchestration_service.ListOrchestrationSitesRequest(
        header=core.MessageHeader(),
        page_size=0  # Server-decide on sizing.
    )
    orchestration_site_list_response = orchestration_site_stub.ListOrchestrationSites(
        orchestration_site_list_request
    )

    print("Orchestration Sites:")
    for site in orchestration_site_list_response.sites:
        print("Site: ({}|{}), type({})".format(site.id.high, site.id.low, site.type))

    site = orchestration_site_list_response.sites[0]
    create_cluster_request = provisioning_service.CreateClusterRequest(
        header=core.MessageHeader(),
        specification=provisioning_service.DeploymentSpecification(
            cluster_size=4,
            model=concord_model.ConcordModelSpecification(
                version="photon-3.0-64",
                # template="4452ea31-fe1c-4e83-b1f7-6aeb12ca9a9b",  # Ubuntu 18.04 Server.
                template="8abc7fda-9576-4b13-9beb-06f867cf2c7c",  # Photon OS 3.0.
                components=[
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                        service_type=concord_model.ConcordComponent.CONCORD,
                        name="vmwblockchain/concord-core:latest"
                    ),
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                        service_type=concord_model.ConcordComponent.ETHEREUM_API,
                        name="vmwblockchain/ethrpc:latest"
                    ),
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                        service_type=concord_model.ConcordComponent.GENERIC,
                        name="vmwblockchain/agent-testing:latest"
                    )
                ]
            ),
            placement=provisioning_service.PlacementSpecification(
                entries=[
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=site.id
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=site.id
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=site.id
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=site.id
                    )
                ]
            ),
            genesis=ethereum.Genesis(
                config=ethereum.Genesis.Config(
                    chain_id=1,
                    homestead_block=0,
                    eip155_block=0,
                    eip158_block=0
                ),
                nonce="0x0000000000000000",
                difficulty="0x400",
                mixhash="0x0000000000000000000000000000000000000000000000000000000000000000",
                parent_hash="0x0000000000000000000000000000000000000000000000000000000000000000",
                gas_limit="0xf4240",
                alloc={
                    "262c0d7ab5ffd4ede2199f6ea793f819e1abb019":
                        ethereum.Genesis.Wallet(balance="12345"),
                    "5bb088f57365907b1840e45984cae028a82af934":
                        ethereum.Genesis.Wallet(balance="0xabcdef"),
                    "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39":
                        ethereum.Genesis.Wallet(balance="0x7fffffffffffffff")
                }
            )
        )
    )
    session_id = provisioning_stub.CreateCluster(create_cluster_request)
    print("CreateCluster(): ", session_id.low, session_id.high)

    get_events_request = provisioning_service.StreamClusterDeploymentSessionEventRequest(
        header=core.MessageHeader(),
        session=session_id
    )
    events = provisioning_stub.StreamClusterDeploymentSessionEvents(get_events_request)
    print("StreamClusterDeploymentSessionEvents():")
    for event in events:
        print("DeploymentEvent: ", event)

    return


if __name__ == "__main__":
    main()
