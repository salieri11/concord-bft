#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
'''
DAML Scenario class

Use the ledger API to create simple assets or to run a trading scenario
that includes multiple assets, parties, actions and permissions.
Based on the dazl Python network stack.
'''

from asyncio import sleep as async_sleep
from logging import info
from os import path as os_path
from sys import path as sys_path
import time

# Submodule location for resources
script_dirname = os_path.dirname(os_path.abspath(__file__))
sys_path.insert(0, os_path.join(script_dirname, 'dazl-client/python'))

from dazl import create as dazl_create, exercise as dazl_exercise


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
    DEFAULT_TIMEOUT = 10                # 10s default timeout for requests
    DEFAULT_POLLING_INTERVAL = 0.01     # 10ms
    DEFAULT_DATA_FILE = os_path.join(script_dirname, 'request_tool/data.yaml')

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
        self.fibo_start = 0
        self.exec_delay = kwargs.get('exec_delay', 1)

    async def run(self):
        '''
        Entry method for starting a scenario run
        '''
        self.cid = {}
        action = [dazl_create('Asset.Quote', self.data['quote_bank']) for n in range(0, self.batch_size)]

        if self.action == 'scenario' and self.full_scenario:
            party = self.data['parties']
            action.extend([dazl_create('Asset.Trader', dict(issuer=party['issuer'], trader=party['trader'])),
                           dazl_create('Asset.Agent', dict(issuer=party['issuer'], agent=party['agent'])),
                           dazl_create('AssetCheck.DiscloseCmd', dict(issuer=party['issuer']))])

            client_action = [dazl_create('Asset.Supervisor', dict(client=party['client'], supervisor=party['supervisor'])),
                             dazl_create('Asset.Clerk', dict(client=party['client'], clerk=party['clerk']))]
            await self.party['client'].submit(client_action)

            # Kick this off while the above are running.
            self.fibo_start = time.time()
            await self.party['issuer'].submit_create_and_exercise('Fibo.InefficientFibonacci',
                                                                  {'owner': self.data['parties']['issuer']},
                                                                  'InefficientFibonacciCompute',
                                                                  {'n': self.exec_delay})
        await self.party['issuer'].submit(action)

    async def asset_count(self, asset='Asset.Quote', party='issuer', match={}):
        '''
        Check if the actual number of valid assets corresponds to the expected number

        Args:
            asset (str): which contract template to search for
            party (str): the role of the user whose assets we're counting
            match (dict): filter by DAML template properties
        '''
        cash = await self.party[party].find_nonempty(asset,
                                                     match,
                                                     min_count=0,
                                                     timeout=self.DEFAULT_TIMEOUT)
        items = len(cash)
        info('Found %d %s records for %s', items, asset, party)

        return items

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
                    await party.submit(dazl_exercise(cid, 'Archive'))
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
                issuer.add_ledger_exercised('Fibo.InefficientFibonacci',
                                            'InefficientFibonacciCompute',
                                            self._exercised_fibonacci)

                agent, trader = self.party['agent'], self.party['trader']
                agent.add_ledger_created('Asset.Agent', self._created_agent)
                agent.add_ledger_created('Asset.Pricing', self._created_pricing)
                trader.add_ledger_created('Asset.Trader', self._created_trader)
                trader.add_ledger_exercised('Asset.Trader', 'Trader_Quote', self._exercised_trader_quote)

                clerk, supervisor = self.party['clerk'], self.party['supervisor']
                clerk.add_ledger_created('Asset.Clerk', self._created_clerk)
                # TODO: commented out because of error:
                # Writer loop for party Charlie has NOT fully finished, but will be terminated anyway (1 futures still pending).
                # clerk.add_ledger_created('Asset.Quote', self._exercised_disclose)
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
        wait_step = self.DEFAULT_POLLING_INTERVAL
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

                return dazl_exercise(event.cid, 'Fill')

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
            return dazl_create('Asset.Fixing', self.data['fixing'])

        info('Could not Fill: %s', event)

    async def _created_fixing(self, event):
        if event.cdata is not None:
            self.cid['Fixing'] = event.cid
            info('Created Fixing: %s', event.cid)
            await self._wait_for('Option', self.cid, self.DEFAULT_TIMEOUT)

            return dazl_exercise(self.cid['Option'], 'Expire', dict(fixingCid=event.cid))

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

            return dazl_exercise(event.cid, 'Agent_Quote', agent_quote)

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
            return dazl_exercise(event.cid, 'Disclose', params)

        info('Could not create Disclose helper: %s', event)

    async def _exercised_disclose(self, event):
        if event.cdata is not None:
            self.cid['Disclosed_Quote'] = event.cid
            info('Disclosed Quote: %s', event.cid)
            await self._wait_for('Clerk', self.cid, self.DEFAULT_TIMEOUT)

            observers = [self.data['parties']['supervisor']]
            params = dict(quoteCid=event.cid, observers=observers)

            return dazl_exercise(self.cid['Clerk'], 'Clerk_Request', params)
        else:
            info('Could not Disclose: %s', event)

    async def _exercised_clerk_request(self, event):
        if event.cdata is not None:
            self.cid['Request'] = event.cid
            info('Created Clerk Request: %s', event.cid)
            params = dict(requestCid=self.cid['Request'])
            return dazl_exercise(self.cid['Supervisor'], 'Supervisor_Accept', params)

        info('Could not create Clerk Request: %s', event)

    async def _exercised_accept(self, event):
        if len(event.exercise_result) == 2:
            info('Complex scenario completed successfully')
        else:
            info('Could not complete complex scenario: %s', event)

    async def _exercised_fibonacci(self, event):
        end = time.time()
        info("Elapsed fibonacci time: {}".format(end - self.fibo_start))
