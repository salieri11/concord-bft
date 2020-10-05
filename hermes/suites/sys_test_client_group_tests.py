##########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# NOTE: The Client HA tests will go in this file and follow the exact
#       same format.  The framework to run these group tests does a lot
#       more than what is used here.
# NOTE: The Client HA tests, last time I ran them, required the gevent
#       monkey patch.  Hermes would only run if I put that at the top
#       of main.py, and doing that broke grpc.  I am not sure how to
#       resolve that outside of reworking the parallel processing
#       in the parties file.  But fortunately, these tests do not do
#       anything in parallel, so not an issue.
##########################################################################

import collections
import json
import pprint
import pytest
import queue
import sys
import time
import urllib.parse

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.helper
import util.daml.party_framework.participants as participants_lib
import util.daml.party_framework.parties as parties_lib

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

PoolPartyFixture = collections.namedtuple("PoolPartyFixture", "ppool, parties")
g_ppool = None
g_parties = None


@pytest.fixture(scope="function")
def fxPoolParty(request, fxBlockchain):
    global g_ppool
    global g_parties

    # This fixture is partly module scope, in that we only want to do initialize once, but
    # also function scope, because we want to clean after every test case.  So a global
    # storage is used to accommodate that.
    if not (g_ppool and g_parties):
        num_parties = 3
        g_ppool, g_parties = create_ppool_and_parties(fxBlockchain, num_parties, request.node.name)

    yield PoolPartyFixture(ppool=g_ppool, parties=g_parties)
    g_parties.clean()


def create_ppool_and_parties(blockchain, num_parties, test_name, bc_id=None):
    '''
    Create the given number of parties and bombardier output directories.
    Returns the ParticipantPool and Parties objects created.
    bc_id can be passed in if we just have a blockchain but no node info.
    '''
    groups = {}
    groups = define_groups(blockchain)
    user, passwd = util.helper.getNodeCredentials()
    ppool = participants_lib.ParticipantPool(groups, user, passwd)
    ppool.wait_for_startup()
    parties = parties_lib.Parties(ppool, num_parties, test_name)
    return ppool, parties


def define_groups(blockchain):
    '''
    blockchain: An fxBlockchain
    Returns a dict of groups and nodes.  e.g.
    {
        "1234-5678-90123": ["1.1.1.1", "1.1.1.2"],
        "1234-5678-90210": ["1.1.1.3", "1.1.1.4"]
    }
    '''
    log.debug("blockchain.replicas: {}".format(blockchain.replicas))
    participant_ips = []

    # Warning about blockchain.replicas["daml_participant"]:
    # If Hermes was passed --replicasConfig, we get an array of IPs.
    # If Hermes did the deployment, we get an array of objects with the IP in one of the fields.
    # Should be changed, might be widespread little changes.
    if isinstance(blockchain.replicas["daml_participant"][0], str):
        participant_ips = blockchain.replicas["daml_participant"]
    else:
        for p in blockchain.replicas["daml_participant"]:
            if "ip" in p and p["ip"]:
                participant_ips.append(p["ip"])
            elif "public_ip" in p and p["public_ip"]:
                participant_ips.append(p["public_ip"])
            elif "private_ip" in p and p["private_ip"]:
                participant_ips.append(p["private_ip"])

    groups = {}

    for ip in participant_ips:
        url_obj = urllib.parse.urlparse("https://{}:6865".format(ip))
        cmd = 'grep "PARTICIPANT_ID" /config/daml-ledger-api/environment-vars | cut -d "=" -f 2'

        # In an end-to-end test, it is possible that the participant node is still being
        # configured, and the above file is not populated yet.  So retry for a while.
        attempts = 30
        sleep_time = 10 # 5 min.
        group = None

        while attempts > 0:
            group = util.helper.ssh_connect(ip, "root", "c0nc0rd", cmd, verbose=False)

            if group and group.strip():
                group = group.strip()
                break
            else:
                attempts -= 1
                msg = "Unable to get group from system '{}'".format(ip)

                if attempts > 0:
                    msg += ", retrying in a few seconds."
                    log.debug(msg)
                    time.sleep(sleep_time)
                else:
                    msg += ", and we are out of retries."
                    raise Exception(msg)

        if group in groups:
            groups[group].append(url_obj)
        else:
            groups[group] = [url_obj]

    summary = "Groups:\n"
    summary += pprint.pformat(groups, indent=4)
    log.debug(summary)
    return groups


@describe()
@pytest.mark.smoke
def test_groups_isolated_write(fxBlockchain, fxConnection, fxPoolParty):
    '''
    Alice and Bob are in different groups.
    Ensure Bob cannot submit a transaction to a participant node in her group.
    '''
    #num_parties = 2
    parties = fxPoolParty.parties
    fleet = parties.get_fleet()
    alice = parties.get_party(0)
    bob = parties.get_party_with_group_affiliation(alice, same_group=False)

    alice.create_tx_threadfn(fleet, 1, 1)
    alice.verify_tx_threadfn(fleet, 1)

    bob.create_tx_threadfn(fleet, 1, 1)
    bob.verify_tx_threadfn(fleet, 1)

    bob.set_participant(alice.get_participant())
    assert bob.verify_contract_creation_failure(fleet), "Should not be able to write."


@describe()
@pytest.mark.smoke
def test_groups_isolated_read(fxBlockchain, fxConnection, fxPoolParty):
    '''
    Alice and Bob are in different groups.
    Bob submits a transaction to his group. Ensure he cannot read it from Alice's group.
    '''
    parties = fxPoolParty.parties
    fleet = parties.get_fleet()
    alice = parties.get_party(0)
    bob = parties.get_party_with_group_affiliation(alice, same_group=False)

    alice.create_tx_threadfn(fleet, 1, 1)
    alice.verify_tx_threadfn(fleet, 1)

    bob.create_tx_threadfn(fleet, 1, 1)
    bob.verify_tx_threadfn(fleet, 1)

    bob.set_participant(alice.get_participant())
    assert bob.verify_contract_read_failure(fleet), "Should not be able to read."


@describe()
@pytest.mark.smoke
def test_can_use_each_node_in_a_group(fxBlockchain, fxConnection, fxPoolParty):
    '''
    Everyone submits transactions to each node in their respective groups and can read all of them
    from all nodes in their groups.
    '''
    parties = fxPoolParty.parties
    parties.send_txs(count=3, connections=3)
    parties.verify_txs(3)
    rotations = 1

    while rotations > 0:
        parties.rotate_parties()
        parties.send_txs(count=3, connections=3)
        parties.verify_txs(3)
        rotations -= 1
