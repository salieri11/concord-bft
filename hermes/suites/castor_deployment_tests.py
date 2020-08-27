# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential

# Castor tests are run after the docker-compose-castor.yml file is launched. That
# docker-compose file launches the deployement. This test further validates that
# the deployment went through successfully.
#
# main.py
#         --dockerComposeFiles=../docker/docker-compose-castor.yml
#         CastorDeploymentTests

import pytest
import atexit
import os
import fnmatch
import re
import json
import subprocess
import util.helper as helper
import util.hermes_logging
from util.product import Product
from suites.case import describe

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
# Set this to no-op so the framework does not wait for Provisioning service to
# start up. It will be started by this test itself.
_productType = helper.TYPE_NO_VERIFY

# NOTE: These need to match the docker-compose-castor.yml file
_CASTOR_DESCRIPTORS_LOC_KEY = "CASTOR_DESCRIPTORS_LOC"
_CASTOR_DESCRIPTORS_LOC_VALUE = "../docker/config-castor/descriptors"
_CASTOR_INFRA_DESCRIPTOR_FILE = "test01_infrastructure_descriptor.json"
_CASTOR_OUTPUT_DIR_KEY = "CASTOR_OUTPUT_DIR"

_COMPOSE_CASTOR_LOG = "docker-compose-castor.log"
_DEPLOYMENT_SUCCESS_MSG = "Deployment completed with status: SUCCESS"
_CASTOR_OUTPUT_SUCCESS_MSG_PATTERN = "Deployment finished at .* with status SUCCESS"
_CASTOR_OUTPUT_NODE_LOGIN_PATTERN = "NODE_LOGIN"
# These must match the infra and deployment descriptor files
_CONSORTIUM_NAME = "hermes-castor-consortium"
_NUM_NODES = 6
_ONPREM_SDDC_4 = "ONPREM SDDC 4"


def match_pattern(lines, pattern):
    matches = 0
    for line in lines:
        if pattern.search(line):
            matches += 1
    return matches


def _populateDescriptorFiles(hermes_settings):
    # Read hermes configs for infrastructure
    url = hermes_settings['zone_config']['zones']['onprem'][0]['api']['address']
    userName = hermes_settings['zone_config']['infra']['SDDC1']['username']
    password = hermes_settings['zone_config']['infra']['SDDC1']['password']
    resource = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['resourcePool']
    storage = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['datastore']
    folder = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['folder']
    networkName = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['network']['name']
    gateway = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['network']['gateway']
    subnet = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['network']['subnet']
    nameServers = hermes_settings['zone_config']['zones']['onprem'][0]['vsphere']['network']['nameServers']

    # Read the infra descriptor file
    infraFilePath = os.path.join(_CASTOR_DESCRIPTORS_LOC_VALUE, _CASTOR_INFRA_DESCRIPTOR_FILE)

    with open(infraFilePath, "r") as infraFile:
        data = infraFile.read()

    infraDescriptor = json.loads(data)

    infraDescriptor['zones'][0]['vCenter']['url'] = url
    infraDescriptor['zones'][0]['vCenter']['userName'] = userName
    infraDescriptor['zones'][0]['vCenter']['password'] = password
    infraDescriptor['zones'][0]['vCenter']['resourcePool'] = resource
    infraDescriptor['zones'][0]['vCenter']['storage'] = storage
    infraDescriptor['zones'][0]['vCenter']['folder'] = folder

    infraDescriptor['zones'][0]['network']['name'] = networkName
    infraDescriptor['zones'][0]['network']['gateway'] = gateway
    infraDescriptor['zones'][0]['network']['subnet'] = subnet
    infraDescriptor['zones'][0]['network']['nameServers'] = nameServers

    # Write back the updated model
    with open(infraFilePath, "w") as infraFile:
        json.dump(infraDescriptor, infraFile, indent=4)

    # DINKARTODO: Remove this log after debugging
    infraJson = json.dumps(infraDescriptor, indent=4)
    log.info("Infra descriptor file was updated with: ")
    log.info(infraJson)


def downCastorDockerCompose(dockerComposeFiles):
    log.info("Stopping docker containers for: %s" % dockerComposeFiles)


@pytest.fixture
def product(hermes_settings):
    cmdLineArgs = hermes_settings["cmdline_args"]
    userConfig = hermes_settings["user_config"]
    product = Product(cmdLineArgs, userConfig)
    return product


@pytest.fixture
def upCastorDockerCompose(hermes_settings, product):
    """
    Launch docker-compose-castor.yml, and wait for it to finish.
    The whole test is predicated on the successful launch and completion of the castor docker process:
    It is a self-contained product that reads the descriptors, calls the provisioning service to deploy
    the blockchain, and exits.

    :return success or failure of launching the compose file:
    """
    _populateDescriptorFiles(hermes_settings);

    dockerComposeFiles = hermes_settings["cmdline_args"].dockerComposeFile
    castorOutputDir = hermes_settings["log_dir"]
    os.makedirs(castorOutputDir, exist_ok=True)

    atexit.register(downCastorDockerCompose, dockerComposeFiles)

    if not product.validatePaths(dockerComposeFiles):
        raise Exception("Docker compose file is not present: %s" % dockerComposeFiles)

    cmd = ["docker-compose"]
    for dcf in dockerComposeFiles:
        cmd += ["--file", dcf]
    cmd += ["up"]
    cmd += ["--abort-on-container-exit"]
    log.info("Launching docker compose: {}".format(cmd))

    # Set up the descriptor and output directories as env variables picked up by docker-compose
    # NOTE: These need to match the docker-compose-castor.yml file
    newEnv = os.environ.copy()
    newEnv[_CASTOR_DESCRIPTORS_LOC_KEY] = _CASTOR_DESCRIPTORS_LOC_VALUE
    newEnv[_CASTOR_OUTPUT_DIR_KEY] = castorOutputDir

    castorComposeOutputLogFilePath = os.path.join(castorOutputDir, _COMPOSE_CASTOR_LOG)
    with open(castorComposeOutputLogFilePath, "a") as composeOutputLogFile:
        # Wait until the docker-compose process terminates
        with subprocess.Popen(cmd,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT,
                              env=newEnv) as proc:
            outData = proc.stdout.read().decode("utf-8")
            composeOutputLogFile.write(outData)

    # Verify that the provisioning process succeeded. Read the output of the docker-compose command and check
    # that is says SUCCESS.
    success = False
    with open(castorComposeOutputLogFilePath) as composeOutputLogFile:
        for logLine in composeOutputLogFile:
            if _DEPLOYMENT_SUCCESS_MSG in logLine:
                success = True
                break

    assert success, "Castor docker-compose startup failed, output does not contain SUCCESS"
    log.info("docker-compose-castor.yml launched")


@describe()
def test_castor_deployment(upCastorDockerCompose, hermes_settings):
    """
        The startup/shutdown of the Castor containers is verified by the upCastorDockerCompose fixture. If that
        fails, there is no point in continuing onward with this test. The test itself only verifies from the output file
        that the clients and committers specified in the descriptors files were provisioned successfully.
    """
    log.info("Starting test test_castor_deployment")
    castorOutputDir = hermes_settings["log_dir"]
    files = os.listdir(castorOutputDir)

    filePatternStr = _CONSORTIUM_NAME + "*"
    outputFileBases = fnmatch.filter(files, filePatternStr)
    assert len(outputFileBases) == 1, "Zero/Multiple matches found for consortium: %s, %s" % (
        _CONSORTIUM_NAME, outputFileBases)

    outputFileBase = outputFileBases[0]
    castorOutputFile = os.path.join(castorOutputDir, outputFileBase)
    log.info("Processing castor output file: {}".format(castorOutputFile))

    # Check that the output file says "SUCCESS"
    outputSuccessMatchPattern = re.compile(_CASTOR_OUTPUT_SUCCESS_MSG_PATTERN)
    with open(castorOutputFile) as cof:
        lines = cof.readlines()
        matchingLines = match_pattern(lines, outputSuccessMatchPattern);
        assert matchingLines == 1, "Castor output file does not have success marker: %s" % _CASTOR_OUTPUT_SUCCESS_MSG_PATTERN

        # Match 2 clients and 4 committers
        nodeLoginMatchPattern = re.compile(_CASTOR_OUTPUT_NODE_LOGIN_PATTERN)
        matchingNodeLogins = match_pattern(lines, nodeLoginMatchPattern)
        assert matchingNodeLogins == _NUM_NODES, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, _NUM_NODES, matchingNodeLogins)

    log.info("test_castor_deployment finished")