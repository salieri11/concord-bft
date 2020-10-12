#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
'''
Simple Fibonacci Tool

Simple wrapper to invoke only the fibonacci algorithm tool in request_tool
'''

from logging import info, WARN, DEBUG
from sys import path as sys_path
import argparse
import asyncio
import os
import time
import yaml

from dazl import Network as dazl_network, setup_default_logger as dazl_setup_logger
from dazl_remote import Remote

# Submodule location for resources
script_dirname = os.path.dirname(os.path.abspath(__file__))
sys_path.insert(0, os.path.join(script_dirname, 'dazl-client/python'))


class SimpleFib:
    # '''
    # DAML SimpleFib runner class
    # Runs just the basic fibonacci app, with options to set the number
    # of iterations and adjust the effort to run them in the DAML ledger.
    # '''
    DATA_FILE = os.path.join(script_dirname, 'request_tool/data.yaml')
    DELAY_SHORT = "short"
    DELAY_LONG = "long"
    DELAY_MIXED = "mixed"
    DELAY_TYPES = [DELAY_SHORT, DELAY_LONG, DELAY_MIXED]

    def __init__(self, parties, data, iterations, num_concurrent, delay_type):
        '''
        Specify the type of scenario to run and what data to use

        Args:
            parties (dict): Dictionary mapping role string to AIOPartyClient objects
            data (dict): Data about parties and trades
            num_iterations (int): Number of iterations
            delay_type: Description of the effort on the ledger api to run each item.
                See SimpleFib.DELAY_TYPES.
        '''
        self.party = parties
        self.data = data
        self.iterations = iterations
        assert delay_type in SimpleFib.DELAY_TYPES
        self.fibo_start = 0
        self._register_triggers()

        if delay_type == SimpleFib.DELAY_SHORT:
            self.exec_delay = 1
        elif delay_type == SimpleFib.DELAY_LONG:
            self.exec_delay = 30

        self.num_concurrent = num_concurrent
        self.to_exercise = []

        self.finished_iteration_items = 0


    async def run(self):
        '''
        Entry method
        '''
        # Always do a short run first because the first time runs overhead code which adds 20-30
        # seconds.
        await self.party['issuer'].submit_create_and_exercise('Fibo.InefficientFibonacci',
                                                              {'owner': self.data['parties']['issuer']},
                                                              'InefficientFibonacciCompute',
                                                              {'n': 0})
        # Now start the real run.
        for i in range(0, self.iterations):
            info("Starting iteration {}".format(i))

            if self.num_concurrent == 1:
                self.fibo_start = time.time()

            concurrent_tasks = []
            for _ in range(0, self.num_concurrent):
                concurrent_tasks.append(self.party['issuer'].submit_create('Fibo.InefficientFibonacci',
                                                                           {'owner': self.data['parties']['issuer']}))
                
            if self.num_concurrent > 1:
                info("***** Launching {} concurrent items".format(self.num_concurrent))

            await asyncio.gather(*concurrent_tasks)

    def _register_triggers(self):
        issuer = self.party['issuer']
        issuer.add_ledger_created('Fibo.InefficientFibonacci',
                                  self._created_fibonacci)
        issuer.add_ledger_exercised('Fibo.InefficientFibonacci',
                                    'InefficientFibonacciCompute',
                                    self._exercised_fibonacci)


    async def _created_fibonacci(self, event):
        if self.num_concurrent == 1:
            self.to_exercise.append(self.party['issuer'].submit_exercise(event.cid,
                                                                         'InefficientFibonacciCompute',
                                                                         {'n': self.exec_delay}))
            
            # self.to_exercise.append(asyncio.sleep(20))
            if len(self.to_exercise) == self.num_concurrent:
                info("**** All creates in.  Sending the exercise commands.")
                await asyncio.gather(*self.to_exercise)
            else:
                info("**** Waiting for more creates to finish.")


    async def _exercised_fibonacci(self, event):
        self.finished_iteration_items += 1
        info("***** Total finished items for this iteration: {}".format(self.finished_iteration_items))



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-u', '--url',
                        default='http://localhost:6865',
                        help='Endpoint URL for the DAML API, format http://host:port')
    parser.add_argument('-r', '--repeat',
                        type=int,
                        default=1,
                        help='Run the command this many times in sequence')
    parser.add_argument('-c', '--num-concurrent',
                        type=int,
                        default=1,
                        help='Run this many instances in each iteration')
    parser.add_argument('-d', '--delay-type',
                        default="short",
                        help="One of {}".format(SimpleFib.DELAY_TYPES))
    args = parser.parse_args()
    print(SimpleFib.DELAY_TYPES)

    with open(SimpleFib.DATA_FILE, "r") as yaml_file:
        data = yaml.load(yaml_file, Loader=yaml.FullLoader)

    dazl_setup_logger(DEBUG)

    remote = Remote(args.url, data['parties'])
    simplefib = SimpleFib(remote.parties,
                          data,
                          args.repeat,
                          args.num_concurrent,
                          args.delay_type)

    remote.network.run_until_complete(simplefib.run())

if __name__ == '__main__':
    main()
