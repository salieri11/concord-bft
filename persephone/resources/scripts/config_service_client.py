import argparse
import grpc
import io
import persephone.core_pb2 as core
import persephone.concord_model_pb2 as concord_model
import persephone.configuration_service_pb2 as configuration_service
import persephone.configuration_service_pb2_grpc as configuration_service_rpc
from typing import Any, Dict


def get_arguments():
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
    argum = get_arguments()
    if argum["trusted_certs"]:
        with io.open(argum["trusted_certs"], "rb") as f:
            trusted_certs = f.read()
        credentials = grpc.ssl_channel_credentials(root_certificates=trusted_certs)
        channel = grpc.secure_channel(argum["server"], credentials)
    else:
        channel = grpc.insecure_channel(argum["server"])

    stub = configuration_service_rpc.ConfigurationServiceStub(channel)
    host_ips = ["10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4"]
    filePath = "/tmp/tlsCerts"
    tls_certs_request = configuration_service.TlsConfigurationServiceRequest(
            header=core.MessageHeader(),
            cert_path=filePath,
            hostIps=host_ips
            )

    tls_certs_response = stub.GenerateTlsConfiguration(tls_certs_request)

    print("generateTlsConfiguration: ", tls_certs_response)

    ethrpc_paths = [filePath + "/node0", filePath + "/node1", filePath + "/node2"]
    etrpc_certs_request = configuration_service.EthRpcConfigurationServiceRequest(
             header=core.MessageHeader(),
             cert_path=ethrpc_paths)

    ethrpc_certs_response = stub.GenerateEthRpcConfiguration(etrpc_certs_request)

    print("generateEthRpcConfiguration: ", ethrpc_certs_response)


if __name__ == "__main__":
    main()
