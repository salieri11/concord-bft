# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

import util.helper as helper

from suites.case import describe
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

from util.daml.concord_daml_client import ConcordDamlClient

import util.hermes_logging

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

# The hex string encodes the DAML SDK command for creating "Party1", as in:
# daml ledger allocate-parties --host localhost --port 6861 Party1
DAML_HEX_CREATE_PARTY_1 = "0804180122671F8B0800000000000000E3B2B2B2105231" \
                          "4D4B494D324C34D635B64836D735313634D74D343734D1" \
                          "4D3348314F3135484E35324B92124849CCCD89CF494D49" \
                          "4F2D8A4F2CC834E4E250620B482C2AA934D4F2E022CA0C" \
                          "21A87A4CB360260100157A201990000000"

# The hex string encodes the DAML SDK command for creating "Party2", as in:
# daml ledger allocate-parties --host localhost --port 6861 Party2
DAML_HEX_CREATE_PARTY_2 = "0804180122671F8B0800000000000000E3B2B2B2105231" \
                          "484E3237324F35D24D34344DD535494C4ED1B54C4D4BD6" \
                          "B534334B4B33493649324A3494124849CCCD89CF494D49" \
                          "4F2D8A4F2CC834E4E250620B482C2AA934D2F2E022CA0C" \
                          "21A87A4CB360260100606C76C490000000"

@describe()
def test_daml_commit_preexecution(fxProduct):
    log.info("Testing pre-execution of DAML requests...")
    daml_client = ConcordDamlClient()

    response = daml_client.commit_transaction(
        submission=bytes.fromhex(DAML_HEX_CREATE_PARTY_1),
        flags=0x0  # normal write (no pre-execution)
    )
    assert response is not None
    last_daml_block_id = response.block_id

    preexecution_response = daml_client.commit_transaction(
        submission=bytes.fromhex(DAML_HEX_CREATE_PARTY_2),
        flags=0x2  # pre-execution enabled
    )
    assert preexecution_response is not None \
           and preexecution_response.block_id > last_daml_block_id, \
        "DAML request pre-execution failed."

    log.info("Pre-execution of DAML requests: OK.")
