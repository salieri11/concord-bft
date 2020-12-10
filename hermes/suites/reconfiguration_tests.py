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
import util.docker_utils as hermes_docker_utils
from . import test_suite
from suites.case import describe
from fixtures.common_fixtures import fxProduct

import util.hermes_logging

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML
MAX_TRIES_TO_PERFORM_AN_ACTION = 10

operator_docker_utils = None
concord_docker_utils = None


@describe()
def test_init(fxProduct, fxHermesRunSettings):
    global operator_docker_utils
    global concord_docker_utils
    operator_docker_utils = hermes_docker_utils.DockerUtils(hermes_docker_utils.operator_containers)
    concord_docker_utils = hermes_docker_utils.DockerUtils(hermes_docker_utils.concord_containers)


def _try_to_perform_an_action(action, stop_condition):
    for i in range(MAX_TRIES_TO_PERFORM_AN_ACTION):
        res = action()
        if stop_condition(res):
            return res
        if i == MAX_TRIES_TO_PERFORM_AN_ACTION - 1:
            log.error("unable to perform reconfiguration actions.")
            assert False
        time.sleep(1)


def _system_has_stopped():
    cmd = "./concop wedge status"
    msg = operator_docker_utils.exec_cmd(id=0, cmd=cmd)
    if msg.lower() == "none":
        return None
    res = json.loads(msg)
    if "succ" in res and not res['succ']:
        return None
    else:
        for v in list(res.values()):
            if not v:
                return False
        return True


def _try_to_wedge():
    cmd = "./concop wedge stop"
    msg = operator_docker_utils.exec_cmd(id=0, cmd=cmd)
    if msg.lower() == "none":
        return None
    else:
        res = json.loads(msg)
        if res["succ"]:
            return True
        else:
            return False

@describe()
def test_wedge_stop_command(fxProduct, fxHermesRunSettings):
    assert _try_to_perform_an_action(_system_has_stopped, lambda _: _ is not None) is False
    assert _try_to_perform_an_action(_try_to_wedge, lambda _: _ is True) is True
    assert _try_to_perform_an_action(_system_has_stopped, lambda _: _ is True) is True
