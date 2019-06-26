#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Thisc file allows one to customize aspects of PyTest.
#################################################################################
import pytest

def pytest_addoption(parser):
    parser.addoption(
        "--hermesCmdlineArgs",
        action="store",
        default="",
        help="Hermes command line options stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesUserConfig",
        action="store",
        default="",
        help="Hermes userConfig object stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesTestLogDir",
        action="store",
        default="",
        help="Hermes testLogDir. POPULATED BY HERMES.")
