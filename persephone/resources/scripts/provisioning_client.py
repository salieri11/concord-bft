#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import logging
import uuid
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.concord_model_pb2 as concord_model
import vmware.blockchain.ethereum.type.genesis_pb2 as genesis
import vmware.blockchain.deployment.v1.orchestration_pb2 as orchestration
import vmware.blockchain.deployment.v1.orchestration_service_pb2 as orchestration_service
import vmware.blockchain.deployment.v1.orchestration_service_pb2_grpc as orchestration_service_rpc
import vmware.blockchain.deployment.v1.provisioning_service_pb2 as provisioning_service
import vmware.blockchain.deployment.v1.provisioning_service_pb2_grpc as provisioning_service_rpc
from google.protobuf.json_format import MessageToJson
from typing import Any, Dict, List


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
    parser.add_argument(
        "--type",
        default="ETHEREUM",
        choices=["ETHEREUM", "DAML", "HLF"],
        help="Type of concord"
    )
    parser.add_argument(
        "--node-type",
        default="NONE",
        choices=["DAML_COMMITTER", "DAML_PARTICIPANT"],
        help="Type of node"
    )
    return vars(parser.parse_args())


def get_component(blockchain_type, node_type) -> List[concord_model.ConcordComponent]:
    """
    Resolve the list of Concord components to deploy for a given blockchain deployment type.

    Args:
        blockchain_type (str): Type of blockchain network to deploy.

    Returns:
        list of Concord components.
    """
    if blockchain_type is None or blockchain_type.upper() == "ETHEREUM":
        return [
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.WAVEFRONT_PROXY,
                name="vmwblockchain/wavefront-proxy:6.1"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.JAEGER_AGENT,
                name="vmwblockchain/jaeger-agent:1.17"
            ),
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
                name="vmwblockchain/agent:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.TELEGRAF,
                name="vmwblockchain/telegraf:1.14.0"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.LOGGING,
                name="vmwblockchain/fluentd:latest"
            )
        ]
    elif blockchain_type.upper() == "DAML":

        if node_type == "DAML_COMMITTER":
            return [
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.WAVEFRONT_PROXY,
                    name="vmwblockchain/wavefront-proxy:6.1"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.JAEGER_AGENT,
                    name="vmwblockchain/jaeger-agent:1.17"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.CONCORD,
                    name="vmwblockchain/concord-core:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_EXECUTION_ENGINE,
                    name="vmwblockchain/daml-execution-engine:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.TELEGRAF,
                    name="vmwblockchain/telegraf:1.14.0"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.GENERIC,
                    name="vmwblockchain/agent:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.LOGGING,
                    name="vmwblockchain/fluentd:latest"
                )
            ]
        elif node_type == "DAML_PARTICIPANT":
            return [
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.WAVEFRONT_PROXY,
                    name="vmwblockchain/wavefront-proxy:6.1"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.TELEGRAF,
                    name="vmwblockchain/telegraf:1.14.0"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_INDEX_DB,
                    name="vmwblockchain/daml-index-db:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_LEDGER_API,
                    name="vmwblockchain/daml-ledger-api:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.GENERIC,
                    name="vmwblockchain/agent:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.LOGGING,
                    name="vmwblockchain/fluentd:latest"
                )
            ]
        else:
            # DAML works of custom images until code it rolled out.
            return [
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_CONCORD,
                    name="vmwblockchain/concord-core:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_EXECUTION_ENGINE,
                    name="vmwblockchain/daml-execution-engine:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_INDEX_DB,
                    name="vmwblockchain/daml-index-db:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.DAML_LEDGER_API,
                    name="vmwblockchain/daml-ledger-api:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.GENERIC,
                    name="vmwblockchain/agent:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.LOGGING,
                    name="vmwblockchain/fluentd:latest"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.WAVEFRONT_PROXY,
                    name="vmwblockchain/wavefront-proxy:6.1"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.JAEGER_AGENT,
                    name="vmwblockchain/jaeger-agent:1.17"
                ),
                concord_model.ConcordComponent(
                    type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                    service_type=concord_model.ConcordComponent.TELEGRAF,
                    name="vmwblockchain/telegraf:1.14.0"
                )
            ]
    elif blockchain_type == "HLF":
        return [
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.HLF_CONCORD,
                name="vmwblockchain/concord-core:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.HLF_ORDERER,
                name="vmwblockchain/hlf-orderer:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.HLF_PEER,
                name="vmwblockchain/hlf-peer:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.HLF_TOOLS,
                name="vmwblockchain/hlf-tools:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.GENERIC,
                name="vmwblockchain/agent:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.WAVEFRONT_PROXY,
                name="vmwblockchain/wavefront-proxy:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.JAEGER_AGENT,
                name="vmwblockchain/jaeger-agent:latest"
            ),
            concord_model.ConcordComponent(
                type=concord_model.ConcordComponent.CONTAINER_IMAGE,
                service_type=concord_model.ConcordComponent.TELEGRAF,
                name="vmwblockchain/telegraf:latest"
            )
        ]


def get_concord_type(blockchain_type: str) -> concord_model.ConcordModelSpecification.BlockchainType:
    """
    Resolve the Concord model specification type for a given blockchain deployment type.

    Args:
        blockchain_type (str): Type of blockchain network to deploy.

    Returns:
        model specification enum type.
    """
    if blockchain_type is None or blockchain_type.upper() == "ETHEREUM":
        return concord_model.ConcordModelSpecification.ETHEREUM
    elif blockchain_type.upper() == "DAML":
        return concord_model.ConcordModelSpecification.DAML
    elif blockchain_type == "HLF":
        return concord_model.ConcordModelSpecification.HLF


def get_node_type(node_type: str) -> concord_model.ConcordModelSpecification.NodeType:
    """
    Resolve the Concord model specification type for a given blockchain deployment type.

    Args:
        node_type (str): Type of Node.

    Returns:
        model specification enum type.
    """
    if node_type is None:
        return concord_model.ConcordModelSpecification.NONE
    elif node_type == "DAML_COMMITTER":
        return concord_model.ConcordModelSpecification.DAML_COMMITTER
    elif node_type == "DAML_PARTICIPANT":
        return concord_model.ConcordModelSpecification.DAML_PARTICIPANT


def main():
    """
    Main program entry-point.

    Example:
        $ python provisioning_client.py --server localhost:9002 --trusted-certs /tmp/server.crt

    Returns:
        None
    """
    # Setup logging.
    logging.basicConfig(
        level=logging.DEBUG,
        format="[%(asctime)s] [%(levelname)s] [%(name)s]: %(message)s",
    )
    log = logging.getLogger("main")

    args = parse_arguments()
    if args["trusted_certs"]:
        with io.open(args["trusted_certs"], "rb") as f:
            trusted_certs = f.read()
        credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
        channel = grpc.secure_channel(args["server"], credentials)
    else:
        channel = grpc.insecure_channel(args["server"])
    provisioning_stub = provisioning_service_rpc.ProvisioningServiceStub(channel)

    site = orchestration.OrchestrationSiteIdentifier(
        id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c")
    create_cluster_request = provisioning_service.CreateClusterRequest(
        header=core.MessageHeader(),
        specification=provisioning_service.DeploymentSpecification(
            cluster_size=4,
            model=concord_model.ConcordModelSpecification(
                version="photon-3.0-64",
                # template="4452ea31-fe1c-4e83-b1f7-6aeb12ca9a9b",  # Ubuntu 18.04 Server.
                template="8abc7fda-9576-4b13-9beb-06f867cf2c7c",  # Photon OS 3.0.
                blockchain_type=get_concord_type(args["type"]),
                node_type=get_node_type(args["node_type"]),
                components=get_component(args["type"], args["node_type"])
            ),
            placement=provisioning_service.PlacementSpecification(
                entries=[
                    getPlacementEntry(site),
                    getPlacementEntry(site),
                    getPlacementEntry(site),
                    getPlacementEntry(site)
                ]
            ),
            properties=core.Properties(
                values={"BLOCKCHAIN_ID":"testBlockchain",
                        "VM_PROFILE":"medium"}
            ),
            genesis=genesis.Genesis(
                config=genesis.Genesis.Config(
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
                        genesis.Genesis.Wallet(balance="12345"),
                    "5bb088f57365907b1840e45984cae028a82af934":
                        genesis.Genesis.Wallet(balance="0xabcdef"),
                    "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39":
                        genesis.Genesis.Wallet(balance="0x7fffffffffffffff")
                }
            ),
            consortium=str(uuid.uuid4())
        )
    )
    log.info("CreateCluster(): request\n{}".format(create_cluster_request))
    session_id = provisioning_stub.CreateCluster(create_cluster_request)
    log.info("CreateCluster(): id(%d)", session_id.id)

    get_events_request = provisioning_service.StreamClusterDeploymentSessionEventRequest(
        header=core.MessageHeader(),
        session=session_id
    )
    events = provisioning_stub.StreamClusterDeploymentSessionEvents(get_events_request)
    log.info("StreamClusterDeploymentSessionEvents(): id(%d)", session_id.id)
    for event in events:
        log.info("DeploymentEvent: %s", MessageToJson(event))

    return

def getPlacementEntry(site):
    # Insert from a file
    return provisioning_service.PlacementSpecification.Entry(
        type=provisioning_service.PlacementSpecification.FIXED,
        site=site,
        site_info=orchestration.OrchestrationSiteInfo(
            type=orchestration.OrchestrationSiteInfo.VMC,
            vmc=orchestration.VmcOrchestrationSiteInfo(
                authentication=core.Endpoint(
                    address="https://console.cloud.vmware.com",
                    credential=core.Credential(
                        token_credential=core.BearerTokenCredential(
                            token="<TOKEN>"
                        )
                    )
                ),
                api=core.Endpoint(
                    address="https://vmc.vmware.com"
                ),
                wavefront=orchestration.Wavefront(
                    url="https://vmware.wavefront.com",
                    token="<TOKEN>"
                ),
                organization="c56e116e-c36f-4f7d-b504-f9a33955b853",
                datacenter="6db19f8f-cde6-4151-88e5-a3b0d6aead6a",
                vsphere=orchestration.VSphereDatacenterInfo(
                    datastore="WorkloadDatastore",
                    resource_pool="Compute-ResourcePool",
                    folder="HermesTesting",
                    network=orchestration.IPv4Network(
                        name="vmware-vpn",
                        address_allocation=orchestration.IPv4Network.STATIC,
                        gateway=172319745,
                        subnet=24
                    )
                ),
                log_managements=[core.LogManagement(
                    destination="LOG_INTELLIGENCE",
                    endpoint=core.Endpoint(
                        address="https://data.mgmt.cloud.vmware.com/le-mans/v1/streams/ingestion-pipeline-stream",
                        credential=core.Credential(
                            token_credential=core.BearerTokenCredential(
                                token="<TOKEN>"
                            )
                        )
                    )
                )]
            )
        )

    )


if __name__ == "__main__":
    main()
