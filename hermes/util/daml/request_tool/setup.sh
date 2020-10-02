#!/bin/bash

# This script is meant to ease test case development.
# It builds the project, allocates parties, and uploads the dar, all
# to the local sandbox ledger.
daml build
daml ledger allocate-parties --host localhost --port 6865
daml ledger upload-dar --host localhost --port 6865 .daml/dist/ledgerclient-0.0.1.dar
