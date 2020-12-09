import click
import re
import os
from utils.read_from_files import read_all_log_files
from utils.reversor import reversor
from datetime import datetime
from typing import TextIO

def add_sorting_value(split_line : list, dst_value : str, src_value : str, config : dict) -> list:
    """Add a sender/receiver value to create the causal order.

    Args:
        split_line (str) : All the values needed to sort the logs in causal order
        dst_value (str) : The sender/receiver value
        src_value (str) : The sender/receiver value
        config (dict): contains the necesary information to parse the specified logs format

    Returns:
        list : A tuple of all the values needed to sort the logs.
    """
    sorting_column = int(config['sorting_column'])
    logger_name_column = 3
    if "st.dst" in split_line[logger_name_column]:
        return split_line[sorting_column].split("-") + [dst_value]
    elif "st.src" in split_line[logger_name_column]:
        return split_line[sorting_column].split("-") + [src_value]
    else:
        return []

def preprocess(logs : list, config : dict) -> list:
    """Preprocess composed of the following:
        1. Filter redundant logs.
        2. Add a sender/recevier value to the logs for causal order.
        The first part of the logs includes logs in which the dest replica logs appear before the src replica,
        whereas the second part is the opposite.

    Args:
        list : The logs from the replicas
        config (dict): contains the necesary information to parse the specified logs format

    """
    sorting_column = int(config['sorting_column'])
    processed_logs = []
    sorting_values_pattern = re.compile(r"^\d+-\d+-\d+-\d+$")

    ignored = ["saveReservedPage", "createCheckpointOfCurrentState",
                    "checkpointReservedPages", "createCheckpointDesc",
                    "deleteOldCheckpoints", "getDigestOfCheckpoint",
                    "startCollectingState", "concord.bft.st.inmem",
                    "concord.bft.st.dbdatastore"]

    for line in logs:
        if ("concord.bft.st" not in line) or any(word in line for word in ignored):
            continue
        split_line = line.split("|")
        
        if sorting_values_pattern.match(split_line[sorting_column]) and "-0-0" in split_line[sorting_column]:
            split_line[sorting_column] = add_sorting_value(split_line,"0", "1", config)
        elif sorting_values_pattern.match(split_line[sorting_column]):
            split_line[sorting_column] = add_sorting_value(split_line,"1", "0", config)
        else:
            continue

        processed_logs.append(split_line)

    return processed_logs

def print_summary(summary : dict , file : TextIO, idx : int) -> None:
    """Prints a summary of the protocol major events
    Args:
        summary (dict): A dictionary that contains information on the protocol's main events
        file (TextIO): A file for writing the logs and summary

    """
    #TODO add the number of ST summary
    file.write("\n***********************\n")
    file.write(f"-SUMMARY fot State Transfer number {idx + 1} -\n")
    sum = [key + val + "\n" for key, val in summary.items()]
    file.write("".join(sum))
    file.write("\t*WARNING*\n")
    file.write("The summary content extracted from the logs,\ntherefore information might be missing.\n")
    file.write("***********************\n\n")
    

def print_to_file(logs : list, out_path : str, summaries : list, config : dict) -> None:
    """Print's the logs and summary of the protocol to a file in a given path
    Args:
        logs (list): list of the causal ordered logs
        out_path (str) : Output file path
        summaries (list) : Contains all the summaries collected for each protocol
        config (dict): contains the necesary information to parse the specified logs format

    """
    sorting_column = int(config['sorting_column'])
    with open(out_path + "st_causal_order.log", "w+") as file:
        for line in logs:
            if type(line[sorting_column]) is list:
                line[sorting_column] = "-".join(line[sorting_column])
            file.write("|".join(line))
            
        for idx,summary in enumerate(summaries):
            print_summary(summary, file, idx)


def find_number(text : str, c : str) -> list:
    """Finds a number in a text, after a given sub-string
    Args:
        text (str) : Text to parse
        c (str) : Substring that appears before the needed number

    Returns:
        A list that contains the number
    """
    return re.findall(r'%s(\d+)' % c, text).pop()

def collect_summary(logs: list, config : dict) -> list: 
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
        config (dict): contains the necesary information to parse the specified logs format

    Returns:
        list : A dictionary that contains a summary of the main events of the protocol
    """
    summaries = list()
    summary = {}
    sorting_column = int(config['sorting_column'])
    dst_replica_column = int(config['dst_replica_column'])
    function_name_column = int(config['function_name_column'])
    msg_column = int(config['msg_column'])
    time_stamp_column = int(config['time_stamp_column'])
    time_stamp_pattern = config['time_stamp_pattern']

    for line in logs:
        try:
            if "sendAskForCheckpointSummariesMsg" in line[function_name_column]:
                if summary and "End Time: " not in summary.keys():
                    summaries.append(summary)
                summary = {}
                summary["Destination replica: "] = line[dst_replica_column]
                summary["Start time: "] = line[time_stamp_column]
            elif "Start fetching checkpoint" in line[msg_column]:
                summary["Source Checkpoint Number: "] = find_number(line[msg_column], "newCheckpoint.checkpointNum: ")
                summary["Source Last Block: "] = find_number(line[msg_column], "newCheckpoint.lastBlock: ")
                summary["Destination Last Reachable Block: "] = find_number(line[msg_column], "lastReachableBlockNum: ")
            elif "Selected new source replica" in line[msg_column]:
                summary["Selected New Source: "] = find_number(line[msg_column], "new source replica: ")
            elif "Invoking onTransferringComplete" in line[msg_column]:
                summary["End Time: "] = line[time_stamp_column]
                end = datetime.strptime(summary["End Time: "], time_stamp_pattern)
                if "Start time: " in summary.keys():
                    start = datetime.strptime(summary["Start time: "], time_stamp_pattern)
                    summary["Duration: "] = str(int((end - start).microseconds / 1000)) + "ms"
                summaries.append(summary)
                summary =  {}
        except IndexError as e:
            print(f"Failed to collect summary, tried to access index out of the log bound, message error: {e}")
        except Exception as e:
            print(f"Failed to collect summary, message error: {e}")

    return summaries

def create_config(is_deploy : str) -> dict:
    """
        Set's all the variables needed to parse the logs according to deployment or local run logs.
    Args:
        deployment (str): A flag that indicates which logs format will be parsed.

    Returns:
        None
    """
    config = dict()
    if is_deploy:
        config['sorting_column'] = "6"
        config['dst_replica_column'] = "2"
        config['function_name_column'] = "8"
        config['msg_column'] = "9"
        config['time_stamp_column'] = "0"
        config['time_stamp_pattern'] = "%Y-%m-%dT%H:%M:%S,%fZ"
    elif not is_deploy:
        config['sorting_column'] = "7"
        config['dst_replica_column'] = "0"
        config['function_name_column'] = "9"
        config['msg_column'] = "10"
        config['time_stamp_column'] = "1"
        config['time_stamp_pattern'] = "%d-%m-%Y %H:%M:%S.%f"
    else:
        raise click.BadParameter(f"'{deployment}', Try 'cli st_parser --help' for help.")
    
    return config

def get_key(log, config):
    sorting_column = int(config['sorting_column'])
    return (int(log[sorting_column][0]), int(log[sorting_column][1]),
            int(log[sorting_column][2]), reversor(int(log[sorting_column][3])),
            int(log[sorting_column][4]))

@click.command('st_parser', short_help='This command parse State Transfer logs')
@click.argument('in_path')
@click.argument('out_path')
@click.option('--is_deploy', default=True, type=bool,
              help='Sets the parser to work on deployment logs. Valid values: true/false')
def parse(in_path : str, out_path : str, is_deploy : bool) -> None:
    """
        \b
        This command parse State Transfer logs
    """
    config = create_config(is_deploy)
    raw_logs = read_all_log_files(in_path)
    processed_logs = preprocess(raw_logs, config)
    ordered_logs = sorted(processed_logs, key=lambda log : get_key(log, config))
    summaries = collect_summary(ordered_logs, config)
    print_to_file(ordered_logs, out_path, summaries, config)
