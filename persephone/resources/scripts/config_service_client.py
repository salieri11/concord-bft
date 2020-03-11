#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import logging
import os
import tempfile
import vmware.blockchain.deployment.v1.core_pb2 as core
import vmware.blockchain.deployment.v1.configuration_service_pb2 as configuration_service
import vmware.blockchain.deployment.v1.configuration_service_pb2_grpc as configuration_service_rpc
from typing import Any, Dict
from urllib.parse import urlparse


def get_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone Configuration Service Client")

    parser.add_argument(
        "--server",
        default="localhost:9003",
        help="Service endpoint (default: localhost:9003)"
    )
    parser.add_argument(
        "--trusted-certs",
        default=None,
        help="File path to trusted server certificates"
    )
    parser.add_argument(
        "--target-path",
        default=None,
        help="Target prefix path to save all generated configuration artifacts"
    )
    parser.add_argument(
        "--hostips",
        nargs="*",
        type=str,
        default=["10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4"],
    )
    ret = vars(parser.parse_args())
    return ret


def main():
    """
    Main program entry-point.

    Example:
        $ python config_service_client.py --server localhost:9003 --trusted-certs /tmp/server.crt

    Returns:
        None
    """
    args = get_arguments()
    if args["trusted_certs"]:
        with io.open(args["trusted_certs"], "rb") as f:
            trusted_certs = f.read()
        credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
        channel = grpc.secure_channel(args["server"], credentials)
    else:
        channel = grpc.insecure_channel(args["server"])

    target_path = args["target_path"]
    if not target_path:
        target_path = os.path.join(tempfile.gettempdir(), "config-output")
    log.info("Target Path Location: %s", target_path)

    stub = configuration_service_rpc.ConfigurationServiceStub(channel)
    host_ips = args["hostips"]
    config_service_request = configuration_service.ConfigurationServiceRequest(
            header=core.MessageHeader(),
            hosts=host_ips
    )

    config_session_id = stub.CreateConfiguration(config_service_request)
    log.info("GenerateConfiguration: %s", config_session_id)

    for i in range(4):
        node_request = configuration_service.NodeConfigurationRequest(
            header=core.MessageHeader(),
            identifier=config_session_id,
            node=i
        )
        node_response = stub.GetNodeConfiguration(node_request)
        log.info("GetNodeConfiguration: %s", node_response)

        for item in node_response.configuration_component:
            if (item.type == 1):
                component_url = urlparse(item.component_url)
                path = os.path.join(os.path.join(target_path, str(i)), component_url.path.strip('/'))
                log.info("Artfact: %s", path)

                if not os.path.exists(os.path.dirname(path)):
                    os.makedirs(os.path.dirname(path), exist_ok=True)
                with open(path, "w+") as f:
                    f.write(item.component)


if __name__ == "__main__":
    # Setup logging.
    log = logging.getLogger("main")
    log.setLevel(logging.DEBUG)
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.DEBUG)
    log_formatter = logging.Formatter("[%(asctime)s] [%(levelname)s] [%(name)s]: %(message)s")
    console_handler.setFormatter(log_formatter)
    log.addHandler(console_handler)

    main()
