import click
import re
import os
from utils.read_from_files import read_all_log_files
from utils.reversor import reversor
from datetime import datetime
from typing import TextIO

def add_sorting_value(sorting_values : str, new_value : str) -> tuple:
    """
        Add a sender/receiver value to create the causal order.
    """
    split_values = sorting_values.split("-")
    split_values.append(new_value)
    return (split_values[0], split_values[1], split_values[2], split_values[3])

def preprocess(logs : list) -> None:
    """
        Preprocess composed of the following:
        Devide the logs to two parts, each one should be sorted differently.
        Add the the sender/receiver of the message, for the causal order.
        Filter redundant logs.
    """
    sorting_col = int(os.getenv('sorting_col'))
    first_part_logs = []
    sec_part_logs = []
    sec_part_pattern = re.compile(r"\d+-\d+-\d+")
    redundants = ["saveReservedPage", "createCheckpointOfCurrentState",
     "checkpointReservedPages", "createCheckpointDesc",
      "deleteOldCheckpoints", "getDigestOfCheckpoint"]

    for line in logs:
        split_line = line.split("|")

        if "st.src" in line and not any(word in line for word in redundants):
            if split_line[sorting_col].isdigit():
                split_line[sorting_col] += "-1"
                first_part_logs.append(split_line)
            elif sec_part_pattern.match(split_line[sorting_col]):
                split_line[sorting_col] = add_sorting_value(split_line[sorting_col], "2")
                sec_part_logs.append(split_line)
        elif "st.dst" in line and not any(word in line for word in redundants):
            if split_line[sorting_col].isdigit():
                split_line[sorting_col] += "-0"
                first_part_logs.append(split_line)
            elif sec_part_pattern.match(split_line[sorting_col]):
                split_line[sorting_col] = add_sorting_value(split_line[sorting_col], "3")
                sec_part_logs.append(split_line)
        else:
            continue

    return first_part_logs, sec_part_logs

def print_summary(summary : dict , file : TextIO):
    file.write("\n***********************\n")
    file.write("\t-SUMMARY-\n")
    sum = [key + val + "\n" for key, val in summary.items()]
    file.write("".join(sum))
    file.write("***********************\n\n")

def print_to_file(logs : list, out_path : str, summaries : list) -> None:
    sorting_col = int(os.getenv('sorting_col'))
    with open(out_path + "st_causal_order.log", "w+") as file:
        for line in logs:
            if type(line[sorting_col]) is tuple:
                line[sorting_col] = "-".join(line[sorting_col])
            file.write("|".join(line))

            if "Invoking onTransferringComplete" in line[10]:
                print_summary(summaries.pop(), file)
            
        if summaries:
            print_summary(summaries.pop(), file)


def find_number(text, c):
    return re.findall(r'%s(\d+)' % c, text).pop()

def collect_summary(logs: list) -> dict: 
    """
        Collect's information from the logs
    """
    summaries = list()
    sorting_col = int(os.getenv('sorting_col'))

    for i,line in enumerate(logs):
        if type(line[sorting_col]) is tuple:
            line[sorting_col] = "-".join(line[sorting_col])
        line = "|".join(line)

        if "sendAskForCheckpointSummariesMsg" in line:
            summary = {}
            summaries.append(summary)
            split_line = line.split("|")
            summary["Destination Replica: "] = split_line[0]
            summary["Start Time: "] = split_line[1]
        elif "Start fetching checkpoint" in line:
            summary["New Checkpoint Number: "] = find_number(line, "newCheckpoint.checkpointNum: ")
            summary["New Checkpoint Last Block: "] = find_number(line, "newCheckpoint.lastBlock: ")
            summary["Last Reachable Block: "] = find_number(line, "lastReachableBlockNum: ")
        elif "Selected new source" in line:
            summary["Selected New Source: "] = line.partition("replica: ")[2][0]
        elif "Invoking onTransferringComplete" in line:
            end_time = line.split("|")[1]
            summary["End Time: "] = end_time
            time_format = "%d-%m-%Y %H:%M:%S.%f"
            start = datetime.strptime(summary["Start Time: "], time_format)
            end = datetime.strptime(end_time, time_format)
            summary["Duration: "] = str(int((end - start).microseconds / 1000)) + "ms"

    return summaries


@click.command('st_parser', short_help='This command parse State Transfer logs')
@click.argument('in_path')
@click.argument('out_path')
def parse(in_path : str, out_path : str) -> None:
    """
        \b
        This command parse State Transfer logs
    """
    sorting_col = 7
    os.environ['sorting_col'] = str(sorting_col)
    raw_logs = read_all_log_files(in_path)
    first_part_logs, sec_part_logs = preprocess(raw_logs)
    ordered_first_part = sorted(first_part_logs, key=lambda log: log[sorting_col])
    ordered_sec_part = sorted(sec_part_logs, key=lambda log: (int(log[sorting_col][0]), int(log[sorting_col][1]), reversor(int(log[sorting_col][2])), int(log[sorting_col][3])))
    orderes_logs = ordered_first_part + ordered_sec_part
    summaries = collect_summary(orderes_logs)
    print_to_file(orderes_logs, out_path, summaries)
