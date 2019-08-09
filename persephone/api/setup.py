#!/usr/bin/env python
# -*- coding: utf-8 -*-

import glob
import io
import os
import pkg_resources

from setuptools import setup, Command

# Package meta-data.
NAME = "persephone-api"
DESCRIPTION = "VMware Blockchain Fleet Management API."
URL = "https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain"
EMAIL = "blockchain-support@vmware.com"
AUTHOR = "VMware Blockchain Team"
REQUIRES_PYTHON = ">=3.7.0"
VERSION = "1.0.1"

# Package dependencies.
REQUIRED = ["grpcio", "grpcio-tools"]
EXTRAS = {}

# Source file path.
path = os.path.abspath(os.path.dirname(__file__))

# Make sure that build target directory exists.
os.makedirs(os.path.join(path, "target"), exist_ok=True)

# Import the README and use it as the long-description.
try:
    with io.open(os.path.join(path, "README.md"), encoding="utf-8") as f:
        long_description = '\n' + f.read()
except FileNotFoundError:
    long_description = DESCRIPTION

# Load the package's __version__.py module as a dictionary.
about = {}
if not VERSION:
    project_slug = NAME.lower().replace("-", "_").replace(" ", "_")
    with io.open(os.path.join(path, project_slug, "__version__.py")) as f:
        exec(f.read(), about)
else:
    about["__version__"] = VERSION


class GrpcToolsCommand(Command):
    """Support setup.py gRPC Protocol Buffer binding generation."""
    description = "Build and publish the package."
    user_options = []
    proto_include = pkg_resources.resource_filename("grpc_tools", "_proto")
    proto_src_path = os.path.join(path, "src", "protobuf")
    proto_target_path = os.path.join(path, "target", "lib")

    @staticmethod
    def status(text: str, bold: bool = False) -> None:
        """
        Print status text.

        Args:
            text (str): Text to be printed.
            bold (bool): Whether the text should be printed in bold.

        Returns:
            None
        """
        if bold:
            print("\033[1m{0}\033[0m".format(text))
        else:
            print(text)

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self) -> None:
        """
        Command action invoked by setup.py.

        Returns:
            None
        """
        self.status("Building Protocol Buffer Python bindings...", True)

        # Import here to avoid pip import errors due to repeated calling setup.py.
        import grpc_tools.protoc

        # Create the package directory.
        os.makedirs(self.proto_target_path, exist_ok=True)

        proto_src_list = glob.glob(
            os.path.join(self.proto_src_path, "**", "*.proto"),
            recursive=True
        )
        for proto_src in proto_src_list:
            self.status("Source file: {}".format(proto_src))

        grpc_tools.protoc.main([
            "grpc_tools.protoc",
            "-I{}".format(self.proto_include),
            "-I{}".format(self.proto_src_path),
            "--python_out={}".format(self.proto_target_path),
            "--grpc_python_out={}".format(self.proto_target_path),
        ] + proto_src_list)


# Main entry point for setup.py.
setup(
    name=NAME,
    version=about["__version__"],
    description=DESCRIPTION,
    long_description=long_description,
    long_description_content_type="text/markdown",
    author=AUTHOR,
    author_email=EMAIL,
    python_requires=REQUIRES_PYTHON,
    url=URL,
    namespace_packages=["vmware", "vmware.blockchain"],
    package_dir={
        "vmware.blockchain.deployment.v1": "target/lib/vmware/blockchain/deployment/v1",
        "vmware.blockchain.ethereum.type": "target/lib/vmware/blockchain/ethereum/type"
    },
    packages=[
        "vmware.blockchain.deployment.v1",
        "vmware.blockchain.ethereum.type"
    ],
    install_requires=REQUIRED,
    extras_require=EXTRAS,
    include_package_data=True,
    license="VMware",
    classifiers=[
        # Trove classifiers (https://pypi.python.org/pypi?%3Aaction=list_classifiers).
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: Implementation :: CPython",
        "Programming Language :: Python :: Implementation :: PyPy"
    ],
    cmdclass={
        "grpc": GrpcToolsCommand,
    }
)
