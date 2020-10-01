#!/bin/bash

daml build
daml ledger allocate-parties --host localhost --port 6865
daml ledger upload-dar --host localhost --port 6865 .daml/dist/ledgerclient-0.0.1.dar
