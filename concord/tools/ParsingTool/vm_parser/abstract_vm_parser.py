import click
import re
import datetime
from collections import defaultdict
from vm_parser.color_printer import ColorPrint as printer
from tqdm import tqdm


class AbstractVmParser:

    def __init__(self, timestamp_format: str, first_state: str, second_state: str,
                 timestamp_regex: str, uid_regex: str):
        self.timestamp_format = timestamp_format
        self.first_state = first_state
        self.second_state = second_state
        self.timestamp_regex = timestamp_regex
        self.uid_regex = uid_regex

    def find_states_by_cid(self, logs: list) -> dict:
        """
        Summery:
            Parse the logs by saving the state and timestamp for each UID and
            prints the parsing progress bar to console.
        """

        cid2states = defaultdict(list)
        cid_pattern = re.compile(self.uid_regex)
        timestamp_pattern = re.compile(self.timestamp_regex)

        for line in tqdm(logs, desc="Parsing logs", bar_format='{l_bar}{bar:50}{r_bar}{bar:-10b}'):
            cid_match = cid_pattern.search(line)
            timestamp_match = timestamp_pattern.search(line)

            if not cid_match:
                continue

            if not timestamp_match:
                click.secho(f"failed to extract timestamp from line: {line}")

            cid = cid_match.group(0).split("=")[-1]
            timestamp = timestamp_match.group(0)

            if self.first_state in line:
                cid2states[cid].append(self.first_state)
            elif self.second_state in line:
                cid2states[cid].append(self.second_state)
            else:
                continue

            cid2states[cid].append(timestamp)

        return cid2states

    def print_failed_requests(self, cid2state: dict) -> None:
        for cid, states in cid2state.items():
            if states.count(self.first_state) > states.count(self.second_state):
                printer.print_fail(cid, self.first_state)
            elif states.count(self.first_state) < states.count(self.second_state):
                printer.print_fail(cid, self.second_state)

    def calc_duration(self, first_timestamp: str, second_timestamp: str) -> int:
        first_datetime = datetime.datetime.strptime(first_timestamp, self.timestamp_format)
        second_datetime = datetime.datetime.strptime(second_timestamp, self.timestamp_format)
        timedelta_millis = (second_datetime - first_datetime).microseconds / 1000
        return int(timedelta_millis)

    def print_exceeded_threshold(self, cid2state: dict, thld: int) -> None:
        for cid, states in cid2state.items():
            for sending_timestamp, submission_timestamp in zip(states[1::2], states[3::2]):
                duration = self.calc_duration(sending_timestamp, submission_timestamp)
                if duration > thld:
                    printer.print_duration(cid, duration, thld)

    def exceeded_threshold(self, logs: list, thld: int) -> None:
        """
            Prints requests which took a long time to submit
        """
        cid2state = self.find_states_by_cid(logs)
        self.print_failed_requests(cid2state)
        self.print_exceeded_threshold(cid2state, thld)

    def failed_requests(self, logs: list) -> None:
        """
            Prints failed requests by finding CID's with missing states.
        """
        cid2states = self.find_states_by_cid(logs)
        self.print_failed_requests(cid2states)
