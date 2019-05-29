#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import persephone.core_pb2 as core
import persephone.concord_model_pb2 as concord_model
import persephone.ethereum_pb2 as ethereum
import persephone.orchestration_pb2 as orchestration
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
    stub = provisioning_service_rpc.ProvisioningServiceStub(channel)

    create_cluster_request = provisioning_service.CreateClusterRequest(
        header=core.MessageHeader(),
        specification=provisioning_service.DeploymentSpecification(
            cluster_size=4,
            model=concord_model.ConcordModelSpecification(
                version="photon-3.0-64",
                template="8abc7fda-9576-4b13-9beb-06f867cf2c7c",
                components=[
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.DOCKER_IMAGE,
                        name="vmwblockchain/concord-core:latest"
                    ),
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.DOCKER_IMAGE,
                        name="vmwblockchain/ethrpc:latest"
                    ),
                    concord_model.ConcordComponent(
                        type=concord_model.ConcordComponent.DOCKER_IMAGE,
                        name="vmwblockchain/agent-testing:latest"
                    )
                ]
            ),
            placement=provisioning_service.PlacementSpecification(
                entries=[
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=orchestration.OrchestrationSiteIdentifier(
                            low=3915407548904001415,
                            high=10241087041514402754
                        )
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=orchestration.OrchestrationSiteIdentifier(
                            low=3915407548904001415,
                            high=10241087041514402754
                        )
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=orchestration.OrchestrationSiteIdentifier(
                            low=3915407548904001415,
                            high=10241087041514402754
                        )
                    ),
                    provisioning_service.PlacementSpecification.Entry(
                        type=provisioning_service.PlacementSpecification.FIXED,
                        site=orchestration.OrchestrationSiteIdentifier(
                            low=3915407548904001415,
                            high=10241087041514402754
                        )
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
    session_id = stub.CreateCluster(create_cluster_request)
    print("CreateCluster(): ", session_id)

    get_events_request = provisioning_service.StreamClusterDeploymentSessionEventRequest(
        header=core.MessageHeader(),
        session=session_id
    )
    events = stub.StreamClusterDeploymentSessionEvents(get_events_request)
    print("StreamClusterDeploymentSessionEvents():")
    for event in events:
        print("DeploymentEvent: ", event)

    return


if __name__ == "__main__":
    main()
