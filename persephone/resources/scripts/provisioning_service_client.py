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
import vmware.blockchain.deployment.v1.provisioning_service_new_pb2 as provisioning_service
import vmware.blockchain.deployment.v1.provisioning_service_new_pb2_grpc as provisioning_service_rpc
from google.protobuf.json_format import MessageToJson
from typing import Any, Dict, List

SITE_ID_1 = "4cbc7fda-9576-4b13-9beb-06f867cf2c7c"
SITE_ID_2 = "acbc7fda-9576-4b13-9beb-06f867cf2c7c"


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone Provisioning Service Deployment Client")
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
        default="DAML",
        choices=["ETHEREUM", "DAML", "HLF"],
        help="Type of concord"
    )
    return vars(parser.parse_args())


def get_blockchain_type(blockchain_type: str) -> core.BlockchainType:
    """
    Resolve the Concord model specification type for a given blockchain deployment type.

    Args:
        blockchain_type (str): Type of blockchain network to deploy.

    Returns:
        model specification enum type.
    """
    if blockchain_type is None or blockchain_type.upper() == "ETHEREUM":
        return core.BlockchainType.ETHEREUM
    elif blockchain_type.upper() == "DAML":
        return core.BlockchainType.DAML
    elif blockchain_type == "HLF":
        return core.BlockchainType.HLF


def main():
    """
    Main program entry-point.

    Example:
        $ python provisioning_service_client.py --server localhost:9002 [--trusted-certs /tmp/server.crt]

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
    provisioning_stub = provisioning_service_rpc.ProvisioningServiceV2Stub(channel)

    site = orchestration.OrchestrationSiteIdentifier(
        id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c")
    create_deployment_request = provisioning_service.DeploymentRequest(
        # header=core.MessageHeader(),
        spec=provisioning_service.DeploymentSpec(
            # consortium_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
            # blockchain_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
            blockchain_type=get_blockchain_type(args["type"]),
            sites=getDeploymentSites(),
            nodeAssignment=provisioning_service.NodeAssignment(
                entries=(
                    [
                        provisioning_service.NodeAssignment.Entry(
                            site=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                            type=0,  # REPLICA
                        # properties=core.Properties()
                        # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        ),
                        provisioning_service.NodeAssignment.Entry(
                            site=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                            type=0,  # REPLICA
                        ),
                        provisioning_service.NodeAssignment.Entry(
                            site=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                            type=0,  # REPLICA
                        ),
                        provisioning_service.NodeAssignment.Entry(
                            site=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                            type=0,  # REPLICA
                        ),
                        provisioning_service.NodeAssignment.Entry(
                            site=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                            type=1,  # CLIENT
                        ),
                        # provisioning_service.NODEASSIGNMENT_ENTRY(
                        #     site=orchestration.OrchestrationSiteIdentifier(
                        #         id=SITE_ID_1),
                        #     type=0,  # REPLICA
                        #     # properties=core.Properties()
                        #     # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        # ),
                        # provisioning_service.NODEASSIGNMENT_ENTRY(
                        #     site=orchestration.OrchestrationSiteIdentifier(
                        #         id=SITE_ID_1),
                        #     type=0,  # REPLICA
                        #     # properties=core.Properties()
                        #     # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        # ),
                        # provisioning_service.NODEASSIGNMENT_ENTRY(
                        #     site=orchestration.OrchestrationSiteIdentifier(
                        #         id=SITE_ID_1),
                        #     type=0,  # REPLICA
                        #     # properties=core.Properties()
                        #     # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        # ),
                        # provisioning_service.NODEASSIGNMENT_ENTRY(
                        #     site=orchestration.OrchestrationSiteIdentifier(
                        #         id=SITE_ID_1),
                        #     type=1,  # REPLICA
                        #     # properties=core.Properties()
                        #     # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        # ),
                        # provisioning_service.NodeAssignmentEntry(
                        #     site=orchestration.OrchestrationSiteIdentifier(
                        #         id=SITE_ID_1),
                        #     type=1,  # REPLICA
                        #     # properties=core.Properties()
                        #     # node_id="4cbc7fda-9576-4b13-9beb-06f867cf2c7c"/str(uuid.uuid4()),
                        # ),
                    ]
                )
            ),
            # properties=core.Properties()
            # node_properties=map<string, core.Properties())
        )
    )
    log.info("Create Deployment(): request\n{}".format(create_deployment_request))
    session = provisioning_stub.CreateDeployment(create_deployment_request)
    log.info("Create Deployment(): id({})".format(session.id))

    get_events_request = provisioning_service.StreamDeploymentSessionEventRequest(
        header=core.MessageHeader(),
        session_id=session.id
    )
    events = provisioning_stub.StreamDeploymentSessionEvents(get_events_request)
    log.info("DeploymentExecutionEvent(): id({})".format(session.id))
    for event in events:
        log.info("DeploymentEvent: {}".format(MessageToJson(event)))

    return


def getDeploymentSites():
    # Insert from a file
    return provisioning_service.Sites(
        info_list=[
            orchestration.OrchestrationSite(
                    id=orchestration.OrchestrationSiteIdentifier(id=SITE_ID_1),
                    info=orchestration.OrchestrationSiteInfo(
                        type=orchestration.OrchestrationSiteInfo.VMC,
                        vmc=orchestration.VmcOrchestrationSiteInfo(
                            authentication=core.Endpoint(
                                address="https://console.cloud.vmware.com",
                                credential=core.Credential(
                                    token_credential=core.BearerTokenCredential(
                                        token="<REFRESH_TOKEN>"
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
                            datacenter="a890ac97-941d-4479-a90c-98061c1e3639",
                            vsphere=orchestration.VSphereDatacenterInfo(
                                datastore="WorkloadDatastore",
                                resource_pool="Compute-ResourcePool",
                                folder="khank",
                                network=orchestration.IPv4Network(
                                    name="sddc1-persephone",
                                    address_allocation=orchestration.IPv4Network.STATIC,
                                    gateway=167772673,
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
        ]

    )


if __name__ == "__main__":
    main()