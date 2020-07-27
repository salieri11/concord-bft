#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import glob
import grpc_tools.protoc
import os
import pkg_resources
from typing import Any, Dict


def parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments.

    Returns:
        a dictionary containing parsed arguments and their associated values.
    """
    parser = argparse.ArgumentParser(description="gRPC Python Binding Generator")
    parser.add_argument(
        "--source-path",
        default=None,
        help="File path to root protocol buffer definition sources"
    )
    parser.add_argument(
        "--target-path",
        default=None,
        help="File path to target generated sources"
    )

    return vars(parser.parse_args())


def generate_bindings(source_path: str, target_path: str) -> None:
    """
    Generate gRPC Python bindings.

    Args:
        source_path (str): path to root directory of source .proto files.
        target_path (str): path to store generated .py files.

    Returns:
        None
    """
    if not source_path or not target_path:
        return

    # Create the package directory.
    os.makedirs(target_path, exist_ok=True)

    proto_src_list = glob.glob(
        os.path.join(source_path, "**", "*.proto"),
        recursive=True
    )
    for proto_src in proto_src_list:
        print("Source file: {}".format(proto_src))

    grpc_tools.protoc.main([
        "grpc_tools.protoc",
        "-I{}".format(pkg_resources.resource_filename("grpc_tools", "_proto")),
        "-I{}".format(source_path),
        "--python_out={}".format(target_path),
        "--grpc_python_out={}".format(target_path),
    ] + proto_src_list)


def main():
    """
    Main program entry-point.

    Example:
        $ python hermes/util/generate_grpc_bindings.py \
            --source-path=<path> \
            --target-path=<path>

    Returns:
        None
    """
    args = parse_arguments()
    generate_bindings(args["source_path"], args["target_path"])


if __name__ == "__main__":
    main()
