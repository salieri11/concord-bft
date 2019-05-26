#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import grpc
import io
import persephone.core_pb2 as core
import persephone.concord_model_pb2 as concord_model
import persephone.metadata_service_pb2 as metadata_service
import persephone.metadata_service_pb2_grpc as metadata_service_rpc
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="Persephone Metadata Service Client")
    parser.add_argument(
        "--server",
        default="localhost:9001",
        help="Service endpoint (default: localhost:9001)"
    )
    parser.add_argument(
        "--trusted-certs",
        help="File path to trusted server certificates"
    )

    return vars(parser.parse_args())


def main():
    """
    Main program entry-point.

    Example:
        $ python metadata_client.py --server localhost:9001 --trusted-certs /tmp/sslcerts/server.crt

    Returns:
        None
    """
    args = parse_arguments()
    with io.open(args["trusted_certs"], "rb") as f:
        trusted_certs = f.read()
    credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
    channel = grpc.secure_channel(args["server"], credentials)
    stub = metadata_service_rpc.ConcordModelServiceStub(channel)

    add_model_request = metadata_service.AddModelRequest(
        header=core.MessageHeader(),
        specification=concord_model.ConcordModelSpecification(
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
                )
            ]
        )
    )
    add_model_response = stub.AddModel(add_model_request)
    print("AddModel(): ", add_model_response)

    list_model_request = metadata_service.ListModelsRequest(
        header=core.MessageHeader(),
        order_by=metadata_service.ListModelsRequest.UNSPECIFIED,
        result_limit=0
    )
    list_models_response = stub.ListModels(list_model_request)
    for event in list_models_response:
        print("ListModels(): ", event)


if __name__ == "__main__":
    main()
