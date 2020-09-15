#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import concurrent.futures
import copy
import datetime
import json
import os
import pprint
import random
import shutil
import time
import yaml

from util.daml.customer.exceptions import TransactionCreationError, TransactionReadError
import util.daml.bombardier
import util.daml.daml_helper
import util.hermes_logging as hermes_logging_util
log = hermes_logging_util.getMainLogger()

# Reduces external dependencies, handy for debugging.
NO_EXEC = False

class Parties:
    '''
    This is a group of parties.
    '''
    names = []
    base_daml_app_path = "util/daml/apps/quickstart"
    daml_sdk_path = None # Set after installing the DAML SDK.

    def __init__(self, ppool, count, test_name):
        '''
        ppool: The pool of participant nodes.
        count: Number of parties to create.
        '''
        self._parties = []
        self._ppool = ppool

        for i in range(0, count):
            Parties.names.append("Customer_Name_{}".format(i))

        log.debug("Names: {}".format(Parties.names))

        if NO_EXEC:
            self._bombardier_fleet = type('obj', (object,), {
                "clients": [
                    {
                        "logFile": "mock_log_file",
                        "port": 1
                    },
                    {
                        "logFile": "mock_log_file",
                        "port": 2
                    },
                    {
                        "logFile": "mock_log_file",
                        "port": 3
                    }
                ]
            })
        else:
            log.info("Starting bombardier fleet")
            self._bombardier_fleet = util.daml.bombardier.BombardierFleet(test_name, count, check_install=True)

        # Organize our group list such that we can distribute parties
        # among groups evenly, and parties within groups among nodes in that
        # group evenly.
        # groups = {
        #     "group_1": {
        #         "participants": [p1, p2, p3],
        #         "parites": [Alice, Bob, ...]
        #     }
        # }
        groups = {}
        for p in ppool.participants:
            if p.group in groups:
                groups[p.group]["participants"].append(p)
            else:
                groups[p.group] = {
                    "participants": [p],
                    "parties": []
                }

        group_name_index = 0
        group_names = list(groups.keys())

        for i in range(0, count):
            name = Parties.names[i]
            party_group = None
            group_name = None

            # Define all of the participants this party can use.
            # Use # parties to help define which the party should use first,
            # so all are not starting on the same node.
            for g in groups:
                if g == group_names[group_name_index]:
                    group_name = g
                    party_group = groups[g]
                    break

            participants = party_group["participants"]
            participant_index = len(groups[g]["parties"]) % len(participants)
            initial_participant = participants[participant_index]
            bombardier_client = self._bombardier_fleet.clients[i]
            party = Party(name, group_name, participants, initial_participant, bombardier_client, Parties.base_daml_app_path)
            self._parties.append(party)
            groups[g]["parties"].append(name)

            if group_name_index < len(group_names)-1:
                group_name_index += 1
            else:
                group_name_index = 0

        group_summary = copy.deepcopy(groups)
        for g in group_summary:
            for p in group_summary[g]["participants"].copy():
                group_summary[g]["participants"].append(p.ip)
                group_summary[g]["participants"].remove(p)

        pprint.pprint(group_summary, indent=4)


    def get_party(self, index):
        '''
        Return the party at the given index.
        '''
        return self._parties[index]


    def get_fleet(self):
        '''
        Return the bombardier fleet for this group of parties.
        '''
        return self._bombardier_fleet


    def clean(self):
        '''
        Tells each party to clean its test data.
        '''
        for party in self._parties:
            party.clean()


    def rotate_parties(self, randomize=False):
        '''
        Make each party use a different participant node.
        Will go in sequential order unless randomize is True.
        '''
        log.info("Changing participants")

        for party in self._parties:
            party.select_next_participant(randomize)


    def send_txs(self, count, connections):
        '''
        Have every Party send <count> transactions using <connections> connections.
        '''
        futures = []
        log.info("Creating transactions.")
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(self._parties)) as executor:
            for party in self._parties:
                future = executor.submit(party.create_tx_threadfn, self._bombardier_fleet, count, connections)
                futures.append(future)

        # This will display exceptions which occur in the threads.
        for f in futures:
            if f.result():
                log.debug(f.result())

        for party in self._parties:
            party.print_transactions()


    def verify_txs(self, connections):
        '''
        Have every Party verify they can pull every txId from the partcipant node they are currently
        assigned to.
        '''
        futures = []
        log.info("Verifying transactions.")
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(self._parties)) as executor:
            for party in self._parties:
                future = executor.submit(party.verify_tx_threadfn, self._bombardier_fleet, connections)
                futures.append(future)

        # This will display exceptions which occur in the threads.
        for f in futures:
            if f.result():
                log.debug(f.result())


    def get_party_with_group_affiliation(self, orig_party, same_group=False):
        '''
        Returns a party related to the given party.
        "Related to" means either in the same or different group as the passed in party.
        '''
        for p in self._parties:
            if not p.get_name() == orig_party.get_name():
                if (p.get_group() == orig_party.get_group()) == same_group:
                    return p


class Party:
    def __init__(self, name, group, participants, initial_participant, bombardier_client, base_daml_app_path):
        '''
        A Party represents a customer with an app which connects to
        the Ledger API server.  As such, each has:
        - A name.
        - A group of participant nodes, aka "client", aka "ledger api server".
        - Its own bombardier server.  Think of this as part of a
          customer's DAML app.
        - Its own DAML app directory (IOU with a daml.yaml file
          customized for the Party.

        A Party is assigned a participant node at first and may
        change which participant node it points to during tests.
        '''
        # All tx ids.
        self._txIds = []

        # For metrics/analysis.  It contains things like time stamps and tps values.
        self._all_load_responses = []

        self._name = name
        self._group = group
        self._participants = participants
        self._participant = initial_participant
        self._original_participant = initial_participant
        self._bombardier_client = bombardier_client
        self._daml_sdk_version = util.daml.daml_helper.get_ledger_api_version(self._participant.ip)
        self._install_daml_sdk()
        self._initialize_daml_project(base_daml_app_path)
        self._party_project_dir = None
        self._vm = None
        log.info("Created party:\n name={}\n participant={}\n bombardier log={}\n bombardier port={}" \
              .format(self._name, self._participant, self._bombardier_client["logFile"],
                      self._bombardier_client["port"]))


    def __str__(self):
       return "Party {}:\n group: {}\n participants: {}\n original_participant: {}". \
            format(self._name, self._group, self._participants, self._original_participant)


    def _install_daml_sdk(self):
        if not Parties.daml_sdk_path:
            Parties.daml_sdk_path = util.daml.daml_helper.install_daml_sdk(self._daml_sdk_version)


    def _initialize_daml_project(self, base_daml_app_path):
        '''
        base_daml_app_path: Location of a daml project which the party uses.
        '''
        self._configure_daml_project(base_daml_app_path)
        self._deploy_daml_app()


    def _deploy_daml_app(self):
        '''
        Use the DAML SDK tool to deploy this Party's app to the participant
        node it has been associated with.
        Note: Requires the DAML SDK.
        '''
        cmd = [Parties.daml_sdk_path, "deploy", "--host", self._participant.ip, "--port", str(self._participant.port)]

        success, output = util.helper.execute_ext_command(cmd, timeout=120,
                                                          working_dir=self._party_project_dir,
                                                          verbose=True)
        if not success and not "Party already exists" in output:
            raise Exception("Unable to deploy DAML app for party {}".format(self._name))


    def _configure_daml_project(self, base_daml_app_path):
        '''
        base_daml_app_path: Location of a daml project which the party uses.
        We are given a generic app that can be used by every party.  We want to
        make slight modifications to make it "owned" by this Party.
        '''
        self._party_project_dir = base_daml_app_path + "_" + self._name

        if os.path.isdir(self._party_project_dir):
            shutil.rmtree(self._party_project_dir)

        shutil.copytree(base_daml_app_path, self._party_project_dir)
        daml_config_file = os.path.join(self._party_project_dir, "daml.yaml")

        with open(daml_config_file, "r") as f:
            daml_config = yaml.load(f, Loader=yaml.Loader)

        daml_config["parties"] = [self._name]
        daml_config["sdk-version"] = self._daml_sdk_version

        with open(daml_config_file, "w+") as f:
            yaml.dump(daml_config, f)


    def clean(self):
        '''
        Clean cached data between test cases and reset the user to
        use its original participant.
        '''
        self._participant = self._original_participant
        log.debug("Clean: {} is back to {}".format(self._name, self._participant))
        self._all_load_responses = []
        self._txIds = []


    def get_group(self):
        return self._group


    def get_name(self):
        return self._name


    def set_participant(self, participant):
        '''
        participant: A Participant object.
        Some tests require the participant node to be changed. e.g. Bob
        needs to verify against Alice's participant node
        '''
        self._participant = participant


    def get_participant(self):
        '''
        Returns the participant object this party is using.
        '''
        return self._participant


    def select_next_participant(self, randomize=False):
        '''
        Change to the next participant in its list.
        (Go to the first if at the last.)
        '''
        msg = "    {} participant shift:\t{} => ".format(self._name, self._participant)
        i = 0

        if len(self._participants) < 2:
            raise Exception("Cannot change a party's participant when there is only one participant.")

        if randomize:
            self._participant = random.choice(self._participants)
        else:
            for participant in self._participants:
                if participant.ip == self._participant.ip:
                    new_participant = None

                    if i == len(self._participants) - 1:
                        new_participant = self._participants[0]
                    else:
                        new_participant = self._participants[i + 1]

                    self._participant = new_participant
                else:
                    i += 1

        msg += str(self._participant)
        log.info(msg)


    def create_read_txs_sequentially(self, bombardier_fleet, count):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.
        count: How many transactions to create and read.
        Just calls the creation and read functions with one tx at a time.
        '''
        for i in range(0, count):
            log.info("{} creating transaction {}".format(self._name, i))
            self.create_tx_threadfn(bombardier_fleet, count=1, connections=1, retry_seconds=600)
            time.sleep(1)

        self.verify_tx_threadfn(bombardier_fleet, connections=10)


    def create_tx_threadfn(self, bombardier_fleet, count, connections, retry_seconds=1, txload_timeout=120):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.
        count: Number of transactions to create.
        connections: Number of connecdtions to create.
        retry_seconds: How long to keep retrying commands. (Node may be rebooting.)
        txload_timeout: How long one bombardier call can last.

        Create transactions, and throw an error on failure.  Verifies the response itself, but
        does not verify the transaction can be read from the blockchain.

        At times, this is called in multiple threads concurrently; be mindful.
        '''
        log.info("  Party {} is creating txs. count={}, connections={}".format(self._name, count, connections))
        elapsed = 0
        start = datetime.datetime.now()
        num_created = 0

        while elapsed < retry_seconds:
            try:
                ip_host = self._participant.ip + ":" + str(self._participant.port)
                response = bombardier_fleet.txLoad(self._bombardier_client,
                                                   ip_host,
                                                   count=count,
                                                   connections=connections,
                                                   party=self._name,
                                                   txIds=True,
                                                   timeout=txload_timeout)
                elapsed = (datetime.datetime.now() - start).total_seconds()
                log.debug("{}: Elapsed time for creating transactions: {}".format(self._name, elapsed))
                self._all_load_responses.append(response)
                log.debug("all responses: {}".format(self._all_load_responses))

                if response and response["result"]["rejected"] == 0:
                    for txId in response["result"]["txIds"]:
                        self._txIds.append(txId)
                    break
                else:
                    details = json.dumps({
                        "party": self._name,
                        "participant": self._participant.ip,
                        "response": str(response)
                    })

                    raise TransactionCreationError("Transaction creation by {} using node {} failed.". \
                                                   format(self._name, self._participant.ip),
                                                   details)
            except TransactionCreationError as e:
                if elapsed < retry_seconds:
                    log.debug("Transaction creation error, but still below the retry time.  Pausing and retrying.")
                    time.sleep(1)
                else:
                    raise


    def add_txs_for_db_growth(self, fleet, mb_to_add):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.
        mb_to_add: How much the indexdb shoud grow.
        '''
        current_mb = self._participant.get_db_size()
        ending_mb = current_mb + mb_to_add
        log.info("Starting size: {}, goal: {}".format(current_mb, ending_mb))

        while current_mb < ending_mb:
            # I have gone up to 7500 with 200 connections.
            self.create_tx_threadfn(fleet, count=2500, connections=100, retry_seconds=1)
            current_mb = self._participant.get_db_size()
            log.info("  - Current_mb: {}".format(current_mb))
            log.info("  - Total num txs added by {}: {}".format(self._name, len(self._txIds)))


    def generate_txId_batches(self):
        '''
        For performance, generate batches of transaction IDs for verification.
        Returns a list of lists of transaction IDs.
        Over 58, we get request errors, even with curl. The request is just too large.
        '''
        max_txIds_per_batch = 58
        txId_batch = []
        txId_batches = []

        for txId in self._txIds:
            txId_batch.append(txId)

            if len(txId_batch) == max_txIds_per_batch:
                txId_batches.append(txId_batch)
                txId_batch = []

        if txId_batch:
            txId_batches.append(txId_batch)

        return txId_batches


    def verify_tx_threadfn(self, bombardier_fleet, connections):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.

        '''
        if len(self._txIds) == 0:
            log.info("{}: No transactions to verify".format(self._name))
            return

        found_txIds = []
        returned_txs = {}
        batches = self.generate_txId_batches()

        for batch in batches:
            log.info("{} verifying {} transactions from {}".format(self._name, len(batch), self._participant.ip))
            ip_host = self._participant.ip + ":" + str(self._participant.port)
            response = bombardier_fleet.txGet(self._bombardier_client,
                                              ip_host,
                                              batch,
                                              self._name,
                                              connections,
                                              120)
            if not response:
                details = "txGet response was {}".format(response)
                details += "\nbombardier_client: {}".format(self._bombardier_client)
                details += "\nconnections: {}".format(connections)
                details += "\nTo retry, launch a bombardier and use: curl 'http://localhost:8865/tx-get?endpoint={}&ids={}&" \
                           "parties={}&concurrency={}".format(str(self._participant), ",".join(batch), self._name, connections)

                details = json.dumps({
                    "party": self._name,
                    "participant": self._participant.ip,
                    "details": details
                })

                raise TransactionReadError("Transaction read by {} using node {} failed.".format(self._name, self._participant.ip),
                                           details)

            returned_txs = {**returned_txs, **response["result"]["transactions"]}

        for txId in returned_txs:
            # Make sure we received a value.
            if returned_txs[txId]:
                found_txIds.append(txId)

            # Make sure we received something this user created.
            if not txId in self._txIds:
                details = json.dumps({
                    "party": self._name,
                    "participant": self._participant.ip,
                    "txId": txId,
                    "details": "An unexpected txId for {} was returned: {}".format(self._name, txId)
                })

                raise TransactionReadError("Transaction read returned an unexpected transaction.", details)

        for txId in self._txIds:
            # Make sure everything this user created was returned.
            if not txId in found_txIds:
                details = json.dumps({
                    "party": self._name,
                    "participant": self._participant.ip,
                    "txId": txId,
                    "details": "An expected txId for {} was not returned: {}".format(self._name, txId)
                })

                raise TransactionReadError("Transaction read did not return an expected transaction.", details)

        log.info("  {} verified returned_txs on {}".format(self._name, self.get_participant().ip))
        # for txId in self._txIds:
        #     log.info("    {}".format(txId))


    def print_transactions(self):
        log.info("Transactions for {}:".format(self._name))
        for txId in self._txIds:
            log.info("  {}".format(txId))


    def stop_services(self, services):
        self._participant.stop_services(services)


    def start_services(self, services):
        self._participant.start_services(services)


    def pause_services(self, services):
        self._participant.pause_services(services)


    def unpause_services(self, services):
        self._participant.unpause_services(services)


    def reboot_node(self):
        '''
        Tell a party to reboot its node.
        '''
        self._participant.reboot()
        self._participant.clean()


    def shutdown_node(self):
        '''
        Returns a vm handle so we can power it on later.  Don't lose it!
        '''
        log.info("{}: Shutting down my node, {}".format(self._name, self._participant))
        self._vm = self._participant.shutdown()
        return self._vm


    def powerup_node(self, vm=None):
        '''
        Accepts a vm handle, such as what is returned by shutdown_node.
        '''
        log.info("{}: Starting my node, {}".format(self._name, self._participant))

        if not vm:
            vm = self._vm

        self._participant.powerup(vm)


    def wait_for_startup(self):
        '''
        Wait for the participant to be ready.
        '''
        self._participant.wait_for_startup()


    def verify_contract_creation_failure(self, bombardier_fleet, timeout=10):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.
        Try to fail to create a contract before the passed in timeout.
        '''
        elapsed = 0
        log.info("Waiting for a transaction failure")

        while elapsed <= timeout:
            try:
                self.create_tx_threadfn(bombardier_fleet, 1, 1)
                elapsed += 1
                log.info("Waiting for transaction write failure")
                time.sleep(3)
            except TransactionCreationError as e:
                log.info(e)
                log.info("INTENTIONAL transaction failure occurred")
                return True

        return False


    def verify_contract_read_failure(self, bombardier_fleet, timeout=10):
        '''
        bombardier_fleet: A util.daml.bombardier.BombardierFleet object.
        Try to fail to read a contract.
        '''
        elapsed = 0
        log.info("Waiting for a transaction read failure")

        while elapsed <= timeout:
            try:
                self.verify_tx_threadfn(bombardier_fleet, 1)
                elapsed += 1
                log.info("Waiting for transaction read failure")
                time.sleep(3)
            except TransactionReadError as e:
                log.info(e)
                log.info("INTENTIONAL transaction failure occurred")
                return True

            return False
