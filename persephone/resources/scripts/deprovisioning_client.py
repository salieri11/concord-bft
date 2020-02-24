#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import logging
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.provisioning_service_pb2 as provisioning_service
import vmware.blockchain.deployment.v1.provisioning_service_pb2_grpc as provisioning_service_rpc
from google.protobuf.json_format import MessageToJson
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone DeProvisioning Service Client")
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
        "--deployment-id",
        help="Deployment Id tuple. ex: (High.Low)"
    )

    return vars(parser.parse_args())


def main():
    """
    Main program entry-point.

    Example:
        $ python deprovisioning_client.py --server localhost:9002 \
            [--trusted-certs /tmp/server.crt] \
            --deployment-id 5678.1234

    Returns:
        None
    """
    # Setup logging.
    log = logging.getLogger("main")
    log.setLevel(logging.DEBUG)
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.DEBUG)
    log_formatter = logging.Formatter("[%(asctime)s] [%(levelname)s] [%(name)s]: %(message)s")
    console_handler.setFormatter(log_formatter)
    log.addHandler(console_handler)

    args = parse_arguments()
    if args["trusted_certs"]:
        with io.open(args["trusted_certs"], "rb") as f:
            trusted_certs = f.read()
        credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
        channel = grpc.secure_channel(args["server"], credentials)
    else:
        channel = grpc.insecure_channel(args["server"])
    provisioning_stub = provisioning_service_rpc.ProvisioningServiceStub(channel)

    deployment_id_tuple = args["deployment_id"].split(".")
    dep = provisioning_service.DeploymentSessionIdentifier(low=int(deployment_id_tuple[1]),
                                                 high=int(deployment_id_tuple[0]))
    update_deployment_request = provisioning_service.UpdateDeploymentSessionRequest(
        header=core.MessageHeader(),
        session=dep,
        action=provisioning_service.UpdateDeploymentSessionRequest.DEPROVISION_ALL
    )
    provisioning_stub.UpdateDeploymentSession(update_deployment_request)
    get_events_request = provisioning_service.StreamClusterDeploymentSessionEventRequest(
        header=core.MessageHeader(),
        session=dep
    )
    events = provisioning_stub.StreamClusterDeploymentSessionEvents(get_events_request)
    log.info("StreamClusterDeploymentSessionEvents(): id(%d|%d)", dep.high, dep.low)
    for event in events:
        log.info("DeploymentEvent: %s", MessageToJson(event))

    return


if __name__ == "__main__":
    main()
