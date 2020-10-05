#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from dazl import Network as dazl_network

class Remote:
    DEFAULT_POLLING_INTERVAL = 0.01     # 10ms

    '''
    Manage remote API connection and configuration
    '''
    def __init__(self, url, parties=None, poll_interval=DEFAULT_POLLING_INTERVAL):
        '''
        Constructor

        Args:
             url(string): Ledger API endpoint in the format http://host:port
             parties(dict): a role:name data mapping for parties
        '''
        self.url = url
        self.network = None
        self.parties = {}
        self.poll_interval = poll_interval
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
        self.network.set_config(url=self.url, poll_interval=self.poll_interval)

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

