# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# Castor tests are run after the docker-compose-orchestrator.yml file is launched. That
# docker-compose file launches the deployment. This test further validates that
# the deployment went through successfully.
#
# main.py
#         --dockerComposeFiles=../docker/docker-compose-orchestrator.yml,
#                              ../docker/docker-compose-orchestrator-prereqs.yml
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
import tempfile
import sys
import io
import time
from datetime import datetime, timezone
import string
import grpc
from lib.persephone.rpc_helper import RPCHelper
from lib.persephone.provisioning_service_new_helper import ProvisioningServiceNewRPCHelper
from lib.persephone.vmware.blockchain.deployment.v1 import core_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import orchestration_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import provisioning_service_new_pb2 as ps_apis
import itertools
from fixtures.common_fixtures import fxBlockchain, fxProduct
from util.castor import castor_helper as cs
from util.castor.castor_helper import upPrereqsDocker, upCastorDockerCompose, castor_teardown, run_on_failure

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
# Set this to no-op so the framework does not wait for Provisioning service to
# start up. It will be started by this test itself.
_productType = helper.TYPE_NO_VERIFY

@describe("Deploy 7 node daml blockchain and verify blockchain health")
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(7, 2, cs._DEPLOY_DESC_FILENAME_VALUE, cs._INFRA_DESC_FILENAME_VALUE, False)], indirect=True)
def test_castor_deployment_7_node(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # Check if deployment started
    
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    log.info("This is {}".format(upCastorDockerCompose))
    num_of_nodes = 9
    log.info("Starting test test_castor_deployment")
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = cs.validate_castor_output_msg(castorOutputDir)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
        cs._CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = cs._get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = cs._get_node_info_list(all_nodes, client_nodes)

    # verify docker containers on each node
    status, error = cs._verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Deploy 4 node daml blockchain and verify blockchain health")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(4, 1, cs._DEPLOY_DESC_FILENAME_VALUE, cs._INFRA_DESC_FILENAME_VALUE, False)],
                         indirect=True)
def test_castor_4_node_deployment(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. Check VM Sizing
    5. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # Check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = cs.validate_castor_output_msg(castorOutputDir)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
        cs._CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = cs._get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = cs._get_node_info_list(all_nodes, client_nodes)

    # validate node size
    for node_ip, node_type in node_info_list.items():
        vm_size = cs.get_deployed_node_size(node_ip)
        if "committer" in node_type:
            cs.validate_node_size(cs._DEPLOY_DESC_FILENAME_VALUE, vm_size, node_ip, "replica")
        if "client" in node_type:
            cs.validate_node_size(cs._DEPLOY_DESC_FILENAME_VALUE, vm_size, node_ip, "client")

    # verify docker containers on each node
    status, error = cs._verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Test multiple zone deployment")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose', [(4, 1, "test02_deployment_descriptor_multiple_zone.json",
                                                    cs._INFRA_DESC_FILENAME_VALUE, True)],
                         indirect=True)
def test_multiple_zone_deployment(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    log.info("Starting test test_castor_deployment")
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = cs.validate_castor_output_msg(castorOutputDir)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
        cs._CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = cs._get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = cs._get_node_info_list(all_nodes, client_nodes)

    # verify docker containers on each node
    status, error = cs._verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Deployment on invalid host")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose', [(4, 1, "test03_deployment_descriptor_invalid_host.json",
                                                    "test03_infrastructure_descriptor_invalid_host.json", False)],
                         indirect=True)
def test_deployment_invalid_host(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings):
    """
    This is a negative test. We don't need teardown here
    Objective - Validate if castor startup is failed because of invalid host end point
    """
    assert upCastorDockerCompose == False, "Somehow Castor docker compose started the deployment. " \
                                           "It should have been failed"


@describe("Verify Max VM Sizing")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(4, 1, "test04_deployment_descriptor_vm_sizing.json", cs._INFRA_DESC_FILENAME_VALUE, False)],
                         indirect=True)
def test_castor_deployment_max_vm_sizing(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh to all the VMs and check if the node size is configured as per deployment descriptor
    """
    # Check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = cs.validate_castor_output_msg(castorOutputDir)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
        cs._CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = cs._get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = cs._get_node_info_list(all_nodes, client_nodes)

    # validate node size
    for node_ip, node_type in node_info_list.items():
        vm_size = cs.get_deployed_node_size(node_ip)
        if "committer" in node_type:
            cs.validate_node_size("test04_deployment_descriptor_vm_sizing.json", vm_size, node_ip, "replica")
        if "client" in node_type:
            cs.validate_node_size("test04_deployment_descriptor_vm_sizing.json", vm_size, node_ip, "client")


#--------------On Demand -----------------------  


@describe("On demand castor deployment")
@pytest.mark.cmdline
@pytest.mark.on_demand_castor_default # IMPORTANT DO NOT DELETE
def test_on_demand_castor_deployment(fxHermesRunSettings, fxBlockchain, run_on_failure):

    # start the test
    log.info("Starting test...")
    num_of_nodes = int(fxHermesRunSettings["hermesCmdlineArgs"].numReplicas)+int(fxHermesRunSettings["hermesCmdlineArgs"].numParticipants)
    #castorOutputDir = hermes_info["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = cs.validate_castor_output_msg(fxBlockchain.castorOutputDir)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
            cs._CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = cs._get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = cs._get_node_info_list(all_nodes, client_nodes)

    # verify docker containers on each node
    status, error = cs._verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list, fxBlockchain.blockchainId)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=fxBlockchain.castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    log.info(fxBlockchain.blockchainId)
