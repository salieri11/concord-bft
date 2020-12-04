import os
import re
import glob

def read_all_log_files(path : str) -> list:
    logs = []
    for file in glob.glob(path + "/*.log"):
        with open(file, 'r') as f:
            logs.extend(f.readlines())
    return logs