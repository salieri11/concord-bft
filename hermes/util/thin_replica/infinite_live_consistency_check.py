#!/usr/bin/env python3

# Monitor produced blocks through the Thin Replica Server on Concord
#
# Note: The script was written to automate testing with docker-compose locally.
# Therefore, we make the assumption that all nodes expose their DAML gRPC
# service on localhost but with a different port starting at 50051.

import argparse
import os
import sys
from threading import Thread
from time import sleep

from trutil import ThinReplica

class UpdateReader(Thread):
  # As long as pop() and append() are used w/o accessing the list itself these
  # operations are thread-safe
  updates = []

  def __init__(self, trid, block_id=0):
    Thread.__init__(self)
    self.trid = trid
    self.start_block_id = block_id
    self.port = "5005" + str(trid)
    self.stub = ThinReplica(port=self.port)
    self.stop = False

  def run(self):
    last_block_id = self.start_block_id
    while True:
      try:
        for update in self.stub.subscribe_to_update_hashes(block_id=last_block_id+1):
          if self.stop:
            return
          UpdateReader.updates.append(
            {"block_id": update.block_id,
             "trid": self.trid,
             "hash": update.hash})
          last_block_id = update.block_id
      except Exception:
        print("Read failure for TRID " + str(self.trid), file=sys.stderr)
        sleep(1)

  def block_data(self, block_id):
    return self.stub.subscribe_to_updates(block_id=block_id).next()

def parser():
  p = argparse.ArgumentParser()
  p.add_argument("--block_id", type=int, default=0,
                 help="Start at the given block id")
  p.add_argument("--num-nodes", type=int, default=4,
                 help="Number of thin replica servers to query")
  p.add_argument("--no-status", action="store_true", default=False,
                 help="Disable status messages and wait for mismatch only")
  return p

def run(argv):
  args = parser().parse_args(argv)

  # Start reader threads
  reader_threads = []
  for trid in range(args.num_nodes):
    reader = UpdateReader(trid + 1, block_id=args.block_id)
    reader.daemon = True
    reader.start()
    reader_threads.append(reader)

  # Make sure all threads are alive
  assert all(list(map(lambda t: t.isAlive(), reader_threads)))

  # Keep track of the last received updates to report progress
  last_valid_block_id = 0
  last_block_ids = {}
  for reader in reader_threads:
    last_block_ids[reader.trid] = 0

  # Temporary cache to hold updates until all threads report for the same block
  # Key: Block ID
  # Value: List of dicts, {"trid":..., "hash":...}
  update_cache = {}

  while True:
    while not UpdateReader.updates:
      sleep(1)

    # Next update
    update = UpdateReader.updates.pop(0)
    block_id = update["block_id"]
    last_block_ids[update["trid"]] = block_id

    # Add update to cache
    no_block_id = dict(filter(lambda x: x[0] != "block_id", update.items()))
    update_cache.setdefault(block_id, []).append(no_block_id)

    # Print status message
    if not args.no_status:
      msg = ""
      for reader in reader_threads:
        msg += " " + str(reader.trid) + " (" + str(last_block_ids[reader.trid]) + ")"
      print("Valid (" + str(last_valid_block_id) + ")" + msg)

    # Keep reading updates if not all threads reported to this block_id yet
    if len(update_cache[block_id]) != args.num_nodes:
      continue

    # Continue if all hashes match
    updates_for_block_id = update_cache.pop(block_id)
    if isConsistent(updates_for_block_id):
      last_valid_block_id = block_id
      continue

    # Print mismatching hash & data
    for reader in reader_threads:
      reader_update = list(filter(lambda x: x["trid"] == reader.trid, updates_for_block_id))[0]
      printBlock(reader_update, reader.block_data(block_id))
      reader.stop = True
      reader.join()

    # Stop
    break

def isConsistent(updates):
  return len(set(map(lambda x: x["hash"], updates))) == 1

def printBlock(update, data):
  print('#' * 80)
  print("block id:" + str(data.block_id))
  print("trid: " + str(update["trid"]))
  print("hash: " + str(update["hash"].hex()))
  if "correlation_id" in dir(data):
    print("correlation id:" + str(data.correlation_id))
  print("data: " + str(data.data))

if __name__ == "__main__":
  run(sys.argv[1:])
