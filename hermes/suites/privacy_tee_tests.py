# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# In order to run Privacy TEE tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         PrivacyTeeTests
#
# Manual testing via grpcurl, e.g.:
#
# setup blockchain 
# $ gen-docker-concord-config.sh docker/config-public/dockerConfigurationInput-tee.yaml
# $ docker-compose -f -compose-tee.yml up -d
#
# concord-tee command to write test block data  
# $ grpcurl -plaintext -d '{ "test_input" : "PrivacySanityTest" }' 127.0.0.1:50051 com.vmware.concord.tee.TeeService/RunTest
#
# create thin replica client as "client_id_*" - should only see "key_all" 
# $ grpcurl -plaintext -d "" -H "client_id:client_id_*" -format text 127.0.0.1:50051 com.vmware.concord.thin_replica.ThinReplica.ReadState
# block_id: 1
# data: <
#  key: "key-all"
#  value: "value-all"
# >
#
"""Test Thin Replica Privacy"""

import logging
import subprocess
import os
import pytest
import itertools
import time
from suites.case import describe

import util.helper as helper
from util.tee.tutil import Tee
from util.thin_replica.trutil import ThinReplica
from suites.thin_replica_server_tests import get_newest_block_id

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

import tee_pb2 as tproto
import util.hermes_logging  

LOG = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE

@pytest.fixture(scope="module")
def setup_test_suite():
    """Setup function for the whole test suite
    """
    return Tee("localhost", "50051")

def get_test_output(tee):
    return tee.run_test().test_output

@describe()
def test_basic_tee_service(fxProduct, setup_test_suite):
    """Basic TEE service 
    """
    tee = setup_test_suite
    output = tee.run_test("x").test_output

    assert "Test Execution Handler received input 'x'" == output

@describe()
def test_privacy_sanity(fxProduct, setup_test_suite):
    """Sending PrivacySanityTest 
    Test Execution Egine will generate mock transaction
    {
      [
        {
          trids: ["client_id_1", "client_id_2", "client_id_3"],
          k: key-123
          v: value-123
        },
        {
          trids: ["client_id_1"],
          k: key-1
          v: value-1
        },
        {
          trids: ["client_id_2"],
          k: key-2
          v: value-2
        },
        {
          trids: ["client_id_1", "client_id_2"],
          k: key-12
          v: value-12
        },
        {
          trids: [],
          k: key-qll
          v: value-all
        }
      ]
    }
    """
    tee = setup_test_suite
    tee.run_test("PrivacySanityTest").test_output

    assert client_filter("1", 50051)
    assert client_filter("2", 50051)
    assert client_filter("3", 50051)
    assert client_filter("4", 50051)
    assert client_filter("5", 50051)

def client_filter(clientnum, port):
    """Check Client only receives approved data
    ex. client_id_3 expected results
    {
        block_id: 1
        data {
            key: "key-all"
            value: "value-all"
        }
        data {
            key: "key-123"
            value: "value-123"
        }
    }
    """
    client_id = "client_id_" + clientnum
    tr = ThinReplica("localhost", port, client_id)
    stream = tr.read_state()
    for result in stream:
        LOG.info(client_id)
        LOG.info(result)
        for kvp in result.data:
            if not ((clientnum.encode() in kvp.key) or (b"all" in kvp.key)):
                return False
    
    return True

@describe()
def test_privacy_writes(fxProduct, setup_test_suite): 
    tr1 = ThinReplica("localhost", "50051", "client_id_1")
    tr2 = ThinReplica("localhost", "50051", "client_id_2")
    tr3 = ThinReplica("localhost", "50051", "client_id_3")
    tr4 = ThinReplica("localhost", "50051", "client_id_4")
    tr5 = ThinReplica("localhost", "50051", "client_id_5")

    bid = get_newest_block_id([tr1])
    LOG.info("last bid: {0}".format(bid))

    tr1_stream = tr1.subscribe_to_updates(block_id=bid, key_prefix=b"")
    tr2_stream = tr2.subscribe_to_updates(block_id=bid, key_prefix=b"")
    tr3_stream = tr3.subscribe_to_updates(block_id=bid, key_prefix=b"")
    tr4_stream = tr4.subscribe_to_updates(block_id=bid, key_prefix=b"")
    tr5_stream = tr4.subscribe_to_updates(block_id=bid, key_prefix=b"")

    tkv1 = tproto.TridKV()
    tkv1.trids.extend(["client_id_1", "client_id_2", "client_id_3"])
    tkv1.key="key-123"
    tkv1.value="value-123"

    tkv2 = tproto.TridKV()
    tkv2.trids.extend(["client_id_1"])
    tkv2.key="key-1"
    tkv2.value="value-1"

    tkv3 = tproto.TridKV()
    tkv3.trids.extend(["client_id_2"])
    tkv3.key="key-2"
    tkv3.value="value-2"

    tkv4 = tproto.TridKV()
    tkv4.trids.extend(["client_id_1", "client_id_2"])
    tkv4.key="key-12"
    tkv4.value="value-12"

    tkv5 = tproto.TridKV()
    tkv5.trids.extend([])
    tkv5.key="key-all"
    tkv5.value="value-all"

    kv_data = tproto.KVData()
    kv_data.trid_kvs.extend([tkv1,tkv2,tkv3,tkv4,tkv5])

    tee = setup_test_suite
    tee.write_block(kv_data)

    tr1_data = itertools.islice(tr1_stream, 1,2)
    tr2_data = itertools.islice(tr2_stream, 1,2)
    tr3_data = itertools.islice(tr3_stream, 1,2)
    tr4_data = itertools.islice(tr4_stream, 1,2)
    tr5_data = itertools.islice(tr5_stream, 1,2)

    assert client_filter_writes("1",tr1_data)
    assert client_filter_writes("2",tr2_data)
    assert client_filter_writes("3",tr3_data)
    assert client_filter_writes("4",tr4_data)
    assert client_filter_writes("5",tr5_data)  

def client_filter_writes(clientnum,kvdata):
    """Check Client only receives approved data with subscribe
    """
    LOG.info("clientnum: {0}".format(clientnum))
    for result in kvdata:
        LOG.info(result)
        for kvp in result.data:
            if not ((clientnum.encode() in kvp.key) or (b"all" in kvp.key)):
                return False
    
    return True

@describe()
def test_privacy_e2e(fxProduct,setup_test_suite):
    """Privacy End 2 End Test 
    hermes test app --(decorated TRS protocol)-->(cpp trc_test_app)--(trc_lib)--(TRCx4)-->Concord/TRS

    - start two trc_test_app passing in a port and client_id
    - trc_test_app
       - connects to thin_replica_library using client_id
       - creates a grpc server on port
          - a decorated thinreplicaserver 
    - send kvdata via TeeService write_block 
    - connect too each trc_test_app as a grpc client
    - compare results 
    """
    LOG.info("test_privacy_e2e")

    #start two test-apps 
    docker_env = helper.get_docker_env()
    test_app_repo = docker_env["trc_test_app_repo"]
    test_app_tag = docker_env["trc_test_app_tag"]

    docker_str = "docker run -d --network docker_default -p {1}:{1} {2}:{3}" \
          " ./test-app/trc_test_app {0}" \
          " {1} 4 1 concord1:50051 concord2:50051 concord3:50051 concord4:50051"
    
    cid1 = "client_id_6"
    cid2 = "client_id_7"
    port1 = 50063
    port2 = 50061

    cmd1 = docker_str.format(cid1,port1,test_app_repo,test_app_tag)
    cmd2 = docker_str.format(cid2,port2,test_app_repo,test_app_tag)
    
    LOG.info(cmd1)
    subprocess.run(cmd1.split(), check=True, timeout=1000, stdout=subprocess.PIPE)

    LOG.info(cmd2)
    c = subprocess.run(cmd2.split(), check=True, timeout=1000, stdout=subprocess.PIPE)

    assert helper.verify_connectivity("localhost",port1)
    assert helper.verify_connectivity("localhost",port2)

    # connect to test-app grpc decorated thinreplica interface
    
    # subscribe from block 1 
    tr1 = ThinReplica("localhost", port1)
    tr1_stream = tr1.subscribe_to_updates(block_id=1, key_prefix=b"")

    tr2 = ThinReplica("localhost", port2)
    tr2_stream = tr2.subscribe_to_updates(block_id=1, key_prefix=b"")
 
    # get both clients in sync 

    # send block data  
    tkv1 = tproto.TridKV()
    tkv1.trids.extend([cid1, cid2])
    tkv1.key=b"lastblockkey"
    tkv1.value=b"lastblockvalue"
    kv_data = tproto.KVData()
    kv_data.trid_kvs.extend([tkv1])
    tee = setup_test_suite
    tee.write_block(kv_data)

    # find last data sent 
    # will be at blockchain head  
    found1 = False
    while (not found1):
      tr1_data = tr1_stream.next()
      LOG.debug("tr1_data:")
      LOG.debug(tr1_data)
      for kv in tr1_data.data:
        if kv.key == b"lastblockkey":
          found1 = True
          break

    found2 = False
    while (not found2):
      tr2_data = tr2_stream.next()
      LOG.debug("tr2_data:")
      LOG.debug(tr2_data)
      for kv in tr2_data.data:
        if kv.key == b"lastblockkey":
          found2 = True
          break

    # privacy test 

    # send data that only cid 1 should see 
    tkv1 = tproto.TridKV()
    tkv1.trids.extend([cid1])
    tkv1.key=b"onlyclient_id_1key"
    tkv1.value=b"onlyclient_id_1value"
    kv_data = tproto.KVData()
    kv_data.trid_kvs.extend([tkv1])
    tee.write_block(kv_data)

    # get the block_id that cid1 finds data 
    blockonly1 = 0
    tr1_data = tr1_stream.next()
    LOG.debug("tr1_data only1:")
    LOG.debug(tr1_data)
    for kv in tr1_data.data:
      if kv.key == b"onlyclient_id_1key":
        blockonly1 = tr1_data.block_id
        break 

    # confirm cid2 does not see data from same block id 
    tr2_data = tr2_stream.next()
    LOG.debug("tr2_data only1:")
    LOG.debug(tr2_data)
    assert(tr2_data.block_id == blockonly1)
    for kv in tr2_data.data:
      assert (kv.key != b"onlyclient_id_1key")

    # unsubscribe requires an additional block for graceful exit 
    tr1.unsubscribe()
    tr2.unsubscribe()

    # send block for gracefull exit from test-app 
    tee.write_block(kv_data)
    return True

