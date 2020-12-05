import os
import re
from glob import glob

def read_all_log_files(path : str) -> list:
    """Read all the logs from the files in a given path.

    Args:
        path (str) : A path to a directory where all the logs files exist.

    Returns:
        list : All the log lines in a list.
    """
    types = ['/*.log.*', '/*.log']
    files = []
    logs = []
    for t in types:
        files.extend(glob(path + t))

    for file in files:
        with open(file, 'r') as f:
            logs.extend(f.readlines())
    return logs