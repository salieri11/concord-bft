import click
import re
import os
from utils.read_from_files import read_all_log_files
from utils.reversor import reversor
from datetime import datetime
from typing import TextIO

def add_sorting_value(split_line : list, dst_value : str, src_value : str) -> list:
    """Add a sender/receiver value to create the causal order.

    Args:
        split_line (str) : All the values needed to sort the logs in causal order
        dst_value (str) : The sender/receiver value
        src_value (str) : The sender/receiver value

    Returns:
        list : A tuple of all the values needed to sort the logs.
    """
    sorting_column = int(os.getenv('sorting_column'))
    logger_name_column = 3
    if "st.dst" in split_line[logger_name_column]:
        return split_line[sorting_column].split("-") + [dst_value]
    elif "st.src" in split_line[logger_name_column]:
        return split_line[sorting_column].split("-") + [src_value]
    else:
        return []

def preprocess(logs : list) -> list:
    """Preprocess composed of the following:
        1. Filter redundant logs.
        2. Add a sender/recevier value to the logs for causal order.
        The first part of the logs includes logs in which the dest replica logs appear before the src replica,
        whereas the second part is the opposite.

    Args:
        list : The logs from the replicas
    """
    sorting_column = int(os.getenv('sorting_column'))
    processed_logs = []
    two_sorting_pattern = re.compile(r"^\d+-\d+$")
    four_sorting_pattern = re.compile(r"^\d+-\d+-\d+-\d+$")
    logs_pattern = re.compile(r'\d+[\|.+\|]+')

    redundants = ["saveReservedPage", "createCheckpointOfCurrentState",
     "checkpointReservedPages", "createCheckpointDesc",
      "deleteOldCheckpoints", "getDigestOfCheckpoint", "startCollectingState"]

    for line in logs:
        if not logs_pattern.match(line):
            continue
        split_line = line.split("|")

        if two_sorting_pattern.match(split_line[sorting_column]) and not any(word in line for word in redundants):
            split_line[sorting_column] += "-0-0"
            split_line[sorting_column] = add_sorting_value(split_line,"0", "1")
            if not split_line[sorting_column]:
                continue
        elif four_sorting_pattern.match(split_line[sorting_column]) and not any(word in line for word in redundants):
            split_line[sorting_column] = add_sorting_value(split_line,"1", "0")
            if not split_line[sorting_column]:
                continue  
        else:
            continue

        processed_logs.append(split_line)

    return processed_logs

def print_summary(summary : dict , file : TextIO) -> None:
    """Prints a summary of the protocol major events
    Args:
        summary (dict): A dictionary that contains information on the protocol's main events
        file (TextIO): A file for writing the logs and summary

    """
    file.write("\n***********************\n")
    file.write("\t-SUMMARY-\n")
    sum = [key + val + "\n" for key, val in summary.items()]
    file.write("".join(sum))
    file.write("***********************\n\n")

def print_to_file(logs : list, out_path : str, summaries : list) -> None:
    """Print's the logs and summary of the protocol to a file in a given path
    Args:
        logs (list): list of the causal ordered logs
        out_path (str) : Output file path
        summaries (list) : Contains all the summaries collected for each protocol

    """

    sorting_column = int(os.getenv('sorting_column'))
    with open(out_path + "st_causal_order.log", "w+") as file:
        for line in logs:
            if type(line[sorting_column]) is list:
                line[sorting_column] = "-".join(line[sorting_column])
            file.write("|".join(line))
            
        for summary in summaries:
            print_summary(summary, file)


def find_number(text : str, c : str) -> list:
    """Finds a number in a text, after a given sub-string
    Args:
        text (str) : Text to parse
        c (str) : Substring that appears before the needed number

    Returns:
        A list that contains the number
    """
    return re.findall(r'%s(\d+)' % c, text).pop()

def collect_summary(logs: list) -> list: 
    """
        Collect's the following information from the logs:
        1. Destination replica number
        2. Start time of the protocol
        3. New CheckPooint Number
        4. The last block of the new checkpoint
        5. Last reachable block
        6. Source replica number
        7. End time of the protocol

    Args:
        protocols (list): list of the logs seperated to each protocol triggerd

    Returns:
        list : A dictionary that contains a summary of the main events of the protocol
    """
    summaries = list()
    summary = {}
    sorting_column = int(os.getenv('sorting_column'))
    function_name_column = 9
    msg_column = 10

    for line in logs:
        try:
            if "sendAskForCheckpointSummariesMsg" in line[function_name_column]:
                if summary and "End Time: " not in summary.keys():
                    summaries.append(summary)
                summary = {}
                summary["Destination replica: "] = line[0]
                summary["Start time: "] = line[1]
            elif "Start fetching checkpoint" in line[msg_column]:
                summary["Source Checkpoint Number: "] = find_number(line[msg_column], "newCheckpoint.checkpointNum: ")
                summary["Source Last Block: "] = find_number(line[msg_column], "newCheckpoint.lastBlock: ")
                summary["Destination Last Reachable Block: "] = find_number(line[msg_column], "lastReachableBlockNum: ")
            elif "Selected source" in line[msg_column]:
                summary["Selected New Source: "] = find_number(line[msg_column], "source replica: ")
            elif "Invoking onTransferringComplete" in line[msg_column]:
                end_time = line[1]
                summary["End Time: "] = end_time
                time_format = "%d-%m-%Y %H:%M:%S.%f"
                if "Start time: " in summary.keys():
                    start = datetime.strptime(summary["Start time: "], time_format)
                    end = datetime.strptime(end_time, time_format)
                    summary["Duration: "] = str(int((end - start).microseconds / 1000)) + "ms"
                summaries.append(summary)
        except IndexError as e:
            print(f"Failed to collect summary, tried to access out of the log bound, message error: {e}")
        except Exception as e:
            print(f"Failed to collect summary, message error: {e}")

    return summaries

@click.command('st_parser', short_help='This command parse State Transfer logs')
@click.argument('in_path')
@click.argument('out_path')
def parse(in_path : str, out_path : str) -> None:
    """
        \b
        This command parse State Transfer logs
    """
    sorting_column = 7
    os.environ['sorting_column'] = str(sorting_column)
    raw_logs = read_all_log_files(in_path)
    processed_logs = preprocess(raw_logs)
    ordered_logs = sorted(processed_logs, key=lambda log: (int(log[sorting_column][0]), int(log[sorting_column][1]),int(log[sorting_column][2]), reversor(int(log[sorting_column][3])), int(log[sorting_column][4])))
    summaries = collect_summary(ordered_logs)
    print_to_file(ordered_logs, out_path, summaries)