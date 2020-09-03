#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
'''
DAML Scenario runner

Send DAML requests to a ledger API specified by http://host:port
'''

from logging import WARN, info
from argparse import ArgumentParser
from asyncio import sleep as async_sleep
from yaml import load, FullLoader
from time import time, sleep as time_sleep
from sys import path as sys_path
from os import path as os_path

# Submodule location for resources
script_dirname = os_path.dirname(os_path.abspath(__file__))
sys_path.insert(0, os_path.join(script_dirname, 'dazl-client/python'))
DEFAULT_DATA_FILE = os_path.join(script_dirname, 'request_tool/data.yaml')
DEFAULT_POLLING_INTERVAL = 0.01     # 10ms

import dazl

class Scenario():
    '''
    DAML Scenario runner class

    Choose between creating a single asset or running a simple or complex trade scenario
    Can run the scenario multiple times with configurable wait time between runs
    '''
    actions = ['asset', 'scenario']
    templates = ['Asset.Quote',
                 'Asset.Option',
                 'Asset.CashEntry',
                 'Asset.Fixing',
                 'Asset.Agent',
                 'Asset.Pricing',
                 'Asset.Trader',
                 'Asset.Clerk',
                 'Asset.Supervisor',
                 'Asset.Request',
                 'AssetCheck.FetchCmd',
                 'AssetCheck.DiscloseCmd']
    DEFAULT_TIMEOUT = 10            # 10s default timeout for requests

    def __init__(self, parties, data, action, **kwargs):
        '''
        Specify the type of scenario to run and what data to use

        Args:
            parties (dict): dictionary mapping role string to AIOPartyClient objects
            data (dict): data about parties and trades
            action (str): 'asset' or 'scenario'
            batch_size (int): how many identical assets to create in a transaction (default 1)
            complex (bool): if true run a complex scenario, otherwise (default) a simplified one
        '''
        self.party = parties
        self.data = data
        self.action = action
        assert action in Scenario.actions
        self.batch_size = kwargs.get('batch_size', 1)
        self.full_scenario = kwargs.get('complex', False)
        self.cid = {}               # Asset:Cid map of created contract IDs
        self.archived_cid = set()   # Set of archived contract IDs
        self._register_triggers()

    async def run(self):
        '''
        Entry method for starting a scenario run
        '''
        self.cid = {}
        action = [dazl.create('Asset.Quote', self.data['quote_bank']) for n in range(0, self.batch_size)]

        if self.action == 'scenario' and self.full_scenario:
            party = self.data['parties']
            action.extend([dazl.create('Asset.Trader', dict(issuer=party['issuer'], trader=party['trader'])),
                           dazl.create('Asset.Agent', dict(issuer=party['issuer'], agent=party['agent'])),
                           dazl.create('AssetCheck.DiscloseCmd', dict(issuer=party['issuer']))])

            client_action = [dazl.create('Asset.Supervisor', dict(client=party['client'], supervisor=party['supervisor'])),
                             dazl.create('Asset.Clerk', dict(client=party['client'], clerk=party['clerk']))]
            await self.party['client'].submit(client_action)

        await self.party['issuer'].submit(action)

    async def verify_quotes(self, num):
        '''
        Check if the actual number of valid assets corresponds to the expected number

        Args:
            num (int): Number of expected valid (not archived) assets
        '''
        cash = await self.party['issuer'].find_nonempty('Asset.Quote',
                                                        {},
                                                        min_count=0,
                                                        timeout=self.DEFAULT_TIMEOUT)
        info('Expected %s Quote records, found %s: ', num, len(cash))
        for cid in cash.keys():
            data = cash[cid]
            info('%s from %s to %s for %s', cid, data['issuer'], data['client'], data['price'])

        return num == len(cash)

    async def cleanup(self):
        '''
        Archive accessible assets from previous runs
        '''
        for party in self.party.values():
            for template in self.templates:
                assets = await party.find_nonempty(template,
                                                   {},
                                                   min_count=0,
                                                   timeout=self.DEFAULT_TIMEOUT)
                if not assets:
                    # The current party has no active assets of this template's type
                    continue

                for cid_str, cid in {str(k):k for k in assets.keys()}.items():
                    await party.submit(dazl.exercise(cid, 'Archive'))
                    await self._wait_for(cid_str, self.archived_cid, self.DEFAULT_TIMEOUT)

    def _register_triggers(self):
        self.party['client'].add_ledger_created('Asset.Quote', self._created_quote)

        if self.action == 'scenario':
            issuer = self.party['issuer']
            issuer.add_ledger_created('Asset.Fixing', self._created_fixing)
            issuer.add_ledger_exercised('Asset.Quote', 'Fill', self._exercised_fill)
            issuer.add_ledger_exercised('Asset.Option', 'Expire', self._exercised_expire)

            if self.full_scenario:
                issuer.add_ledger_created('AssetCheck.DiscloseCmd', self._created_disclosecmd)

                agent, trader = self.party['agent'], self.party['trader']
                agent.add_ledger_created('Asset.Agent', self._created_agent)
                agent.add_ledger_created('Asset.Pricing', self._created_pricing)
                trader.add_ledger_created('Asset.Trader', self._created_trader)
                trader.add_ledger_exercised('Asset.Trader', 'Trader_Quote', self._exercised_trader_quote)

                clerk, supervisor = self.party['clerk'], self.party['supervisor']
                clerk.add_ledger_created('Asset.Clerk', self._created_clerk)
                clerk.add_ledger_created('Asset.Quote', self._exercised_disclose)
                supervisor.add_ledger_created('Asset.Supervisor', self._created_supervisor)
                supervisor.add_ledger_created('Asset.Request', self._exercised_clerk_request)
                supervisor.add_ledger_exercised('Asset.Supervisor', 'Supervisor_Accept', self._exercised_accept)

        # Cleanup triggers
        for party in self.party.values():
            for template in self.templates:
                party.add_ledger_exercised(template, 'Archive', self._cleaned)

    async def _cleaned(self, event):
        archived = str(event.cid)
        if archived not in self.archived_cid:
            info('Archived contract with ID %s', archived)
        self.archived_cid.add(archived)

    async def _wait_for(self, item, container, seconds):
        wait_step = DEFAULT_POLLING_INTERVAL
        for _ in range(0, int(seconds / wait_step)):
            if item in container:
                return
            await async_sleep(wait_step)

    async def _verify_cash(self):
        cash = self.party['client'].find_active('Asset.CashEntry')
        info('Found %d CashEntry records: ', len(cash))
        for cid in cash.keys():
            data = cash[cid]
            info('%s from %s to %s for %s', cid, data['issuer'], data['client'], data['amount'])

    async def _created_quote(self, event):
        if event.cdata is not None:
            direct = not event.cdata['observers']
            key = 'Quote' if direct else 'Agent_Quote'
            if key in self.cid:
                return

            self.cid[key] = event.cid
            info('Created %s: %s', key, event.cid)

            if direct:
                # Simple scenario continues with a bank-owned Quote
                if self.action == 'asset':
                    # Asset scenario ends here
                    return

                return dazl.exercise(event.cid, 'Fill')

            # Complex scenario continues with a trader Quote
            params = dict(quoteCid=event.cid, price=self.data['quote_bank']['price'])
            await self.party['trader'].submit_exercise(self.cid['Trader'], 'Trader_Quote', params)
        else:
            info('Could not create Quote: %s', event)

    async def _exercised_fill(self, event):
        if 'Option' in self.cid:
            # Complex scenario ends with a Fill
            return

        if len(event.exercise_result) == 2:
            self.cid['Option'] = event.exercise_result['_1']
            self.cid['CashEntry_1'] = event.exercise_result['_2']
            info('Filled Quote: %s', event.cid)
            return dazl.create('Asset.Fixing', self.data['fixing'])

        info('Could not Fill: %s', event)

    async def _created_fixing(self, event):
        if event.cdata is not None:
            self.cid['Fixing'] = event.cid
            info('Created Fixing: %s', event.cid)
            await self._wait_for('Option', self.cid, self.DEFAULT_TIMEOUT)

            return dazl.exercise(self.cid['Option'], 'Expire', dict(fixingCid=event.cid))

        info('Could not create Fixing: %s', event)

    async def _exercised_expire(self, event):
        self.cid['CashEntry_2'] = event.exercise_result
        info('Expired Option: %s', self.cid['Option'])
        await self._verify_cash()

    async def _created_trader(self, event):
        if event.cdata is not None:
            self.cid['Trader'] = event.cid
            info('Created Trader: %s', event.cid)
        else:
            info('Could not create Trader: %s', event)

    async def _created_agent(self, event):
        if event.cdata is not None:
            self.cid['Agent'] = event.cid
            info('Created Agent: %s', event.cid)

            agent_quote = dict(quoteId=self.data['quote_bank']['quoteId'],
                               client=self.data['parties']['client'],
                               optionData=self.data['quote_bank']['optionData'],
                               pricingData=self.data['quote_agent']['pricingData'],
                               price=self.data['quote_agent']['price'],
                               trader=self.data['parties']['trader'],
                               observers=[])

            return dazl.exercise(event.cid, 'Agent_Quote', agent_quote)

        info('Could not create Agent: %s', event)

    async def _created_supervisor(self, event):
        if event.cdata is not None:
            self.cid['Supervisor'] = event.cid
            info('Created Supervisor: %s', event.cid)
        else:
            info('Could not create Supervisor: %s', event)

    async def _created_clerk(self, event):
        if event.cdata is not None:
            self.cid['Clerk'] = event.cid
            info('Created Clerk: %s', event.cid)
        else:
            info('Could not create Clerk: %s', event)

    async def _created_pricing(self, event):
        if event.cdata is not None:
            self.cid['Pricing'] = event.cid
            info('Created Pricing: %s', event.cid)
        else:
            info('Could not create Pricing: %s', event)

    async def _exercised_trader_quote(self, event):
        self.cid['Trader_Quote'] = event.exercise_result
        info('Created Trader Quote: %s', event.cid)

    async def _created_disclosecmd(self, event):
        if event.cdata is not None:
            self.cid['DiscloseCmd'] = event.cid
            info('Created Disclose helper: %s', event.cid)
            await self._wait_for('Trader_Quote', self.cid, self.DEFAULT_TIMEOUT)

            observers = [self.data['parties']['clerk'], self.data['parties']['supervisor']]
            params = dict(quoteCid=self.cid['Trader_Quote'], observers=observers)
            return dazl.exercise(event.cid, 'Disclose', params)

        info('Could not create Disclose helper: %s', event)

    async def _exercised_disclose(self, event):
        if event.cdata is not None:
            self.cid['Disclosed_Quote'] = event.cid
            info('Disclosed Quote: %s', event.cid)
            await self._wait_for('Clerk', self.cid, self.DEFAULT_TIMEOUT)

            observers = [self.data['parties']['supervisor']]
            params = dict(quoteCid=event.cid, observers=observers)
            return dazl.exercise(self.cid['Clerk'], 'Clerk_Request', params)
        else:
            info('Could not Disclose: %s', event)

    async def _exercised_clerk_request(self, event):
        if event.cdata is not None:
            self.cid['Request'] = event.cid
            info('Created Clerk Request: %s', event.cid)
            params = dict(requestCid=self.cid['Request'])
            return dazl.exercise(self.cid['Supervisor'], 'Supervisor_Accept', params)

        info('Could not create Clerk Request: %s', event)

    async def _exercised_accept(self, event):
        if len(event.exercise_result) == 2:
            info('Complex scenario completed successfully')
        else:
            info('Could not complete complex scenario: %s', event)

class Remote:
    '''
    Manage remote API connection and configuration
    '''
    def __init__(self, url):
        self.url = url
        self.network = None
        self.parties = {}

    def connect(self):
        '''
        Establish connection with the remote API (client node)
        '''
        if self.network:
            self.network.shutdown()
            self.network.join()

        self.network = dazl.Network()
        self.network.set_config(url=self.url, poll_interval=DEFAULT_POLLING_INTERVAL)

    def create_clients(self, parties):
        '''
        Create asynchronous clients to connect to the API

        Args:
            parties(dict): a role:name data mapping for parties
        '''
        if not self.network:
            self.connect()

        self.parties = {k:self.network.aio_party(v) for k, v in parties.items()}
        return self.parties

def simple_request(url, minutes, wait=1):
    '''
    Invoke DAML requests with minimal parameters

    Args:
        url(string): Ledger API in the format http://host:port
        minutes(float): duration (minutes) for which perform the requests
        wait(float): timeout (seconds) before running each request
    '''
    with open(DEFAULT_DATA_FILE, "r") as yaml_file:
        data = load(yaml_file, Loader=FullLoader)

    def create_objects(url, data):
        remote = Remote(url)
        remote.connect()
        remote.create_clients(data['parties'])

        scenario = Scenario(remote.parties, data, 'asset')
        return remote, scenario

    remote, scenario = create_objects(url, data)

    end_time = time() + minutes * 60

    while time() < end_time:
        try:
            daml_request(remote, scenario, int((end_time - time()) / wait) + 1, wait)
        except Exception as e:
            # Network error; event loop was broken
            # Create a new network connection and a scenario with new clients
            info('Retrying because of network failure: %s', e)
            remote, scenario = create_objects(url, data)
            time_sleep(1)
            return

def daml_request(remote, scenario, repeat=1, wait=1, cleanup=False):
    '''
    Invoke a DAML request with custom parameters

    Args:
        remote(object): a valid Remote object
        scenario(object): a valid Scenario object
        repeat(int): number of iterations to run the Scenario
        wait(float): seconds to wait before starting a new iteration
        cleanup(bool): if True, archive accessible assets from previous runs
    '''
    async def run_once():
        await async_sleep(wait)
        await scenario.run()
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

    remote.network.run_until_complete(run_request())

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
                        default=DEFAULT_DATA_FILE,
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

    dazl.setup_default_logger(WARN)

    remote = Remote(args.url)
    remote.connect()
    remote.create_clients(data['parties'])

    scenario = Scenario(remote.parties,
                        data,
                        args.action,
                        batch_size=args.batch if args.action == 'asset' else 1,
                        complex=args.complex if args.action == 'scenario' else False)

    daml_request(remote, scenario, args.repeat, args.wait, args.cleanup)

if __name__ == '__main__':
    main()
