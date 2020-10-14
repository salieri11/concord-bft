# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run DAML tests we need a special docker-compose and concord
# configuration file.
#
# /main.py ReconfigurationTests
#               --runConcordConfigurationGeneration
#               --concordConfigurationInput /concord/config/dockerConfigurationInput-reconfiguration.yaml
#               --dockerComposeFile ../docker/docker-compose-daml-nano.yml ../docker/docker-compose-operator.yml

import logging
import os
import pytest
import subprocess
import time
import traceback
import docker
import json
import util.helper as helper
import time
import util.daml.daml_helper as daml_helper

from . import test_suite
from suites.case import describe
from fixtures.common_fixtures import fxProduct

import util.hermes_logging

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML


def _system_has_stopped(operator_container):
    cmd = "./concop wedge status"
    output = operator_container.exec_run(cmd)
    assert output[0] == 0
    msg = output[1].decode('utf-8').rstrip().replace("'", '"')
    if msg.lower() == "none":
        return False
    res = json.loads(msg)
    for v in list(res.values()):
        if v.lower() == "false":
            return False
    return True

@describe()
def test_wedge_stop_command(fxProduct, fxHermesRunSettings):
    client = docker.from_env()
    operator_container = client.containers.get("docker_operator_1")
    assert _system_has_stopped(operator_container) is False
    cmd = "./concop wedge stop"
    output = operator_container.exec_run(cmd)
    msg = output[1].decode('utf-8').rstrip().replace("'", '"').replace('True', '"true"').replace('False', '"false"')
    assert msg.lower() != "none"
    res = json.loads(msg)
    assert res["succ"].lower() == "true"

    # Wait for the system to stop
    tries = 0
    system_stopped = _system_has_stopped(operator_container)
    while system_stopped is False and tries < 10:
        time.sleep(1)
        tries += 1
        system_stopped = _system_has_stopped(operator_container)

    # Verify the system has stopped
    assert _system_has_stopped(operator_container) is True

