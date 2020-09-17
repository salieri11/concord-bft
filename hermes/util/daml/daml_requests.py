#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
'''
DAML Request Tool

Provides a simple interface for DAML requests to a ledger API. 
Based on the dazl Python network stack.
'''

from argparse import ArgumentParser
from logging import WARN, info
from asyncio import sleep as async_sleep
from time import time, sleep as time_sleep
from yaml import load, FullLoader

from scenario import Scenario

from dazl import Network as dazl_network, setup_default_logger as dazl_setup_logger

class Remote:
    '''
    Manage remote API connection and configuration
    '''
    def __init__(self, url, parties=None):
        '''
        Constructor

        Args:
             url(string): Ledger API endpoint in the format http://host:port
             parties(dict): a role:name data mapping for parties
        '''
        self.url = url
        self.network = None
        self.parties = {}
        if parties is not None:
            self.create_clients(parties)

    def connect(self):
        '''
        Establish connection with the remote API (client node)
        '''
        if self.network:
            self.network.shutdown()
            self.network.join()

        self.network = dazl_network()
        self.network.set_config(url=self.url, poll_interval=Scenario.DEFAULT_POLLING_INTERVAL)

    def create_clients(self, parties):
        '''
        Create asynchronous clients to connect to the API

        Args:
            parties(dict): a role:name data mapping for parties

        Returns:
            dict: mapping of role strings to AIOPartyClient objects
        '''
        if not self.network:
            self.connect()

        self.parties = {k:self.network.aio_party(v) for k, v in parties.items()}
        return self.parties

def simple_request(url, requests=1, wait=1, retries=3):
    '''
    Invoke DAML requests with minimal parameters

    Args:
        url(string): Ledger API in the format http://host:port
        requests(int): number of requests to perform, default 1
        wait(float): timeout (seconds) before running each request, default 1
        retries(int): number of times to retry (in case of network errors, etc)

    Returns:
        bool: True if the requests equal created assets, False otherwise
    '''
    with open(Scenario.DEFAULT_DATA_FILE, "r") as yaml_file:
        data = load(yaml_file, Loader=FullLoader)

    def create_objects(url, data):
        remote = Remote(url, data['parties'])
        scenario = Scenario(remote.parties, data, 'asset')
        return remote, scenario

    assets = 0
    def count_assets(_):    # Callback invoked on asset creation
        nonlocal assets
        assets += 1

    remote, scenario = create_objects(url, data)

    for _ in range(0, retries):
        try:
            scenario.party['client'].add_ledger_created('Asset.Quote', count_assets)
            daml_request(remote, scenario, requests, wait, True)
            return requests == assets
        except Exception as e:
            # Network error; event loop was broken
            # Create a new network connection and a scenario with new clients
            info('Retrying because of network failure: %s', e)
            remote, scenario = create_objects(url, data)
            time_sleep(wait)

    return False

def daml_request(remote, scenario, repeat=1, wait=1, cleanup=False):
    '''
    Invoke a DAML request with custom parameters

    Args:
        remote(object): a valid Remote object
        scenario(object): a valid Scenario object
        repeat(int): number of iterations to run the Scenario
        wait(float): seconds to wait before starting a new iteration
        cleanup(bool): if True, archive assets from previous runs

    Returns:
        int: number of active assets for this user
    '''
    async def run_once():
        await scenario.run()
        await async_sleep(wait)
        remote.network.join()

    async def run_request():
        # Wait for all agents to be fully initialized
        for agent in remote.parties.values():
            await agent.ready()

        if cleanup:
            # Archive accessible assets from previous runs
            await scenario.cleanup()

        if repeat > 0:
            for _ in range(0, repeat):
                await run_once()
        else:
            while True:
                await run_once()

        return await scenario.asset_count()
        
    # NB: the asyncio run_until_complete returns the task return value
    # The dazl implementation returns None - colect output using callbacks
    remote.network.run_until_complete(run_request())
    remote.network.join()

def parse_args():
    '''
    Create a parser and process user input

    Invoked in case the request tool is used standalone
    '''
    parser = ArgumentParser(description='Run a DAML scenario')
    parser.add_argument('-u', '--url',
                        default='http://localhost:6865',
                        help='Endpoint URL for the DAML API, format http://host:port')
    parser.add_argument('-d', '--data-file',
                        default=Scenario.DEFAULT_DATA_FILE,
                        help='YAML file with party and asset data')
    subparser = parser.add_subparsers(dest='action',
                                      required=True,
                                      help='Create an asset or run a trade scenario')
    asset = subparser.add_parser('asset',
                                 help='Create an asset')
    asset.add_argument('--batch',
                       type=int,
                       default=1,
                       help='Number of assets batched in a single transaction')
    scenario = subparser.add_parser('scenario',
                                    help='Run trade scenario - create, transfer and archive assets')
    scenario.add_argument('--complex',
                          default=False,
                          action='store_true',
                          help='Run a more complex version of the scenario with additional steps')
    parser.add_argument('-r', '--repeat',
                        type=int,
                        default='1',
                        help='Run the command this many times in sequence')
    parser.add_argument('-w', '--wait',
                        type=float,
                        default=1,
                        help='Seconds to wait between each DAML request')
    parser.add_argument('--cleanup',
                        default=False,
                        action='store_true',
                        help='Do not archive issuer assets from previous runs')
    return parser.parse_args()

def main():
    '''
    Run a scenario as a standalone tool

    Actions:
     - Create parser
     - Load data
     - Connect to remote API and allocate parties
     - Run scenario

    Preconditions:
     - DAR should be uploaded beforehand
     - Ledger parties should be allocated beforehand
    '''
    args = parse_args()

    with open(args.data_file, "r") as yaml_file:
        data = load(yaml_file, Loader=FullLoader)

    dazl_setup_logger(WARN)

    remote = Remote(args.url, data['parties'])
    scenario = Scenario(remote.parties,
                        data,
                        args.action,
                        batch_size=args.batch if args.action == 'asset' else 1,
                        complex=args.complex if args.action == 'scenario' else False)

    daml_request(remote, scenario, args.repeat, args.wait, args.cleanup)

if __name__ == '__main__':
    main()
