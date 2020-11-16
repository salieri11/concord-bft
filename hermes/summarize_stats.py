#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import argparse
import copy
import datetime
import ipaddress
import json
import os
import pprint
import subprocess

import util.stats_gatherer as sg

init_stats = {
    "mem": {
        "min": 0,
        "max": 0
    },
    "cpu": {
        "min": 0,
        "max": 0
    },
    "disk": {
        "min": 0,
        "max": 0
    }
}

# No metric currently provides this for concord VMs.
CONCORD_FREQ_MHZ = 2300

def main():
    arg_parser = argparse.ArgumentParser("Test Suite Hardware Stats Gatherer")
    arg_parser.add_argument("--job",
                            help="Name of the Jenkins job ('master' or 'main')")
    arg_parser.add_argument("--job-url",
                           help="Url to the Jenkins job (excluding build number) if not master or main")
    arg_parser.add_argument("--build",
                            help="Build number")
    args = arg_parser.parse_args()
    
    stats_files = get_files(args.job, args.job_url, args.build)

    for stat_file in stats_files:
        # stat_file looks like ".../DamlTests_20201110_130427/test_logs/stats.json"
        print("Analyzing file {}".format(stat_file))
        suite = os.path.dirname(stat_file)
        suite = os.path.dirname(suite)
        suite = os.path.basename(suite)
        suite = suite.split("_")[0]

        with open(stat_file, "r") as f:
            stats = json.load(f)

        runner_stats = summarize_runner_stats(stats)
        concord_stats = summarize_concord_stats(stats)

        all_stats = summarize_all_stats(runner_stats, concord_stats)
        write_all_stats(suite, runner_stats, concord_stats, all_stats)


def get_files(job, job_url, number):
    '''
    Uses jenkinslogs to fetch the artifacts, then unzips the stats.json files.
    job: The Jenkins job "main" or "master"
    job_url: The url to the Jenkins job, when not using main or master
    number: The Jenkins job number
    Returns a list of json files to process.
    '''
    try:
        subprocess.check_output(["jenkinslogs"])
    except FileNotFoundError as e:
        print("Jenkinslogs needs to be installed and configured to be on the $PATH.  See https://gitlab.eng.vmware.com/rmuschner/jenkinslogs")
        exit(1)

    print("Fetching logs for run {}".format(number))
    new_env = os.environ.copy()
        
    if job:
        cmd = ["jenkinslogs", "-j", job, "logs", "-p", "*zip*/archive.zip", number]
    elif job_url:
        new_env["JENKINS_JOB_URL"] = job_url
        cmd = ["jenkinslogs", "logs", "-p", "*zip*/archive.zip", number]
    else:
        print("No job specified.")
        exit(1)
        
    subprocess.check_output(cmd, env=new_env)
    print("Unzipping...")
    cmd = ["unzip", "-o", "archive-{}.zip".format(number), "**/*/stats.json"]
    subprocess.check_output(cmd)

    stats_files = []

    for root, dirs, files in os.walk("archive"):
        for f in files:
            if f == "stats.json":
              print(os.path.join(root, f))
              stats_files.append(os.path.join(root, f))

    return stats_files


def summarize_runner_stats(stats):
    '''
    Gather stats for the test runner.
    stats: A dict of raw data. See hermes/util/stats_gatherer.py, _gather_local_stats() for format.
    Returns the summarized test runner stats.
    '''
    runner_stats = copy.deepcopy(init_stats)
    corrected_stats = {}

    if not "localhost" in stats:
        # It is possible for runner data to have not been moved to "localhost".
        corrected_stats["localhost"] = {}

        for time_key in stats:
            try:
                new_key = datetime.datetime.fromisoformat(time_key).timestamp()
                corrected_stats["localhost"][new_key] = copy.deepcopy(stats[time_key])
            except Exception as e:
                # It was some other kind of key; ignore.
                print("Exception fixing up runner stats: {}".format(e))
                pass

        stats = corrected_stats

    for timestamp in stats["localhost"]:
        disk = stats["localhost"][timestamp][sg.KEY_DISK_USAGE]["/"]
        runner_stats["disk"]["min"] = select_min_value(disk, runner_stats["disk"]["min"])
        if disk > runner_stats["disk"]["max"]:
            runner_stats["disk"]["max"] = disk

        add_mem_values_to_summary(stats["localhost"][timestamp],
                                  runner_stats)

        add_cpu_values_to_summary(stats["localhost"][timestamp],
                                  runner_stats,
                                  stats["localhost"][timestamp][sg.KEY_CPU_DATA][sg.KEY_CPU_FREQ])

    return runner_stats


def select_min_value(new_val, stored_val):
    '''
    Little util for stats processing of the "min" values.
    '''
    if new_val < stored_val or stored_val == 0:
        return new_val
    else:
        return stored_val


def add_cpu_values_to_summary(stats_dict, summary_dict, cpu_freq):
    '''
    stats_dict: The original dict of all stats
    summary_dict: The dict of summary values which is modified
    cpu_freq: Frequency of the CPU
    '''
    if sg.KEY_CPU_DATA in stats_dict:
        if sg.KEY_CPU_TOTAL in stats_dict[sg.KEY_CPU_DATA]:
            cpu = stats_dict[sg.KEY_CPU_DATA][sg.KEY_CPU_TOTAL]
        else:
            cpu = stats_dict[sg.KEY_CPU_DATA][sg.KEY_CPU_USAGE_AVG]

        cpu *= cpu_freq/100
    else:
        cpu = 0

    cpu = round(cpu, 2)

    summary_dict["cpu"]["min"] = select_min_value(
        cpu, summary_dict["cpu"]["min"])
    if cpu > summary_dict["cpu"]["max"]:
        summary_dict["cpu"]["max"] = cpu


def add_mem_values_to_summary(stats_dict, summary_dict):
    '''
    stats_dict: The original dict of all stats
    summary_dict: The dict of summary values which is modified
    '''
    if sg.KEY_MEM_DATA in stats_dict:
        mem = stats_dict[sg.KEY_MEM_DATA][sg.KEY_MEM_UNAVAIL]
    else:
        mem = 0

    summary_dict["mem"]["min"] = select_min_value(
        mem, summary_dict["mem"]["min"])
    if mem > summary_dict["mem"]["max"]:
        summary_dict["mem"]["max"] = mem


def summarize_concord_stats(stats):
    '''
    Gather stats for the test runner.
    stats: A dict of raw data. See hermes/util/stats_gatherer.py, _gather_local_stats() for format.
    Returns the summarized test runner stats.
    '''
    concord_stats = {}

    for concord_ip in stats:
        try:
            # Ignore "localhost" and any new timestamps present due to timing issues
            # while a run was shutting down.
            ipaddress.ip_address(concord_ip)
        except ValueError:
            continue

        concord_stats[concord_ip] = copy.deepcopy(init_stats)

        for timestamp in stats[concord_ip]:
            if sg.KEY_DISK_USAGE in stats[concord_ip][timestamp]:
                disk = round(stats[concord_ip][timestamp][sg.KEY_DISK_USAGE]["/mnt"] + \
                             stats[concord_ip][timestamp][sg.KEY_DISK_USAGE]["/mnt/data"], 2)
            else:
                disk = 0

            concord_stats[concord_ip]["disk"]["min"] = select_min_value(
                disk, concord_stats[concord_ip]["disk"]["min"])
            if disk > concord_stats[concord_ip]["disk"]["max"]:
                concord_stats[concord_ip]["disk"]["max"] = disk

            add_mem_values_to_summary(stats[concord_ip][timestamp],
                                      concord_stats[concord_ip])

            add_cpu_values_to_summary(stats[concord_ip][timestamp],
                                      concord_stats[concord_ip],
                                      CONCORD_FREQ_MHZ)

    return concord_stats


def summarize_all_stats(runner_stats, concord_stats):
    '''
    Returns a summary of all stats addd together.
    '''
    all_stats = {
        "all": copy.deepcopy(runner_stats)
    }

    for concord_ip in concord_stats:
        all_stats["all"]["mem"]["min"] += concord_stats[concord_ip]["mem"]["min"]
        all_stats["all"]["mem"]["max"] += concord_stats[concord_ip]["mem"]["max"]
        all_stats["all"]["cpu"]["min"] += concord_stats[concord_ip]["cpu"]["min"]
        all_stats["all"]["cpu"]["max"] += concord_stats[concord_ip]["cpu"]["max"]
        all_stats["all"]["disk"]["min"] += concord_stats[concord_ip]["disk"]["min"]
        all_stats["all"]["disk"]["max"] += concord_stats[concord_ip]["disk"]["max"]


    all_stats["all"]["mem"]["min"] = round(all_stats["all"]["mem"]["min"], 2)
    all_stats["all"]["mem"]["max"] = round(all_stats["all"]["mem"]["max"], 2)
    all_stats["all"]["cpu"]["min"] = round(all_stats["all"]["cpu"]["min"], 2)
    all_stats["all"]["cpu"]["max"] = round(all_stats["all"]["cpu"]["max"], 2)
    all_stats["all"]["disk"]["min"] = round(all_stats["all"]["disk"]["min"], 2)
    all_stats["all"]["disk"]["max"] = round(all_stats["all"]["disk"]["max"], 2)

    return all_stats


def write_all_stats(suite, runner_stats, concord_stats, all_stats):
    '''
    Print a report.
    suite: The test suite
    *_stats: The stats dicts created elsewhere in this utility.
    '''
    print("Result for suite {}".format(suite))
    print("  Runner stats:")
    print("    cpu min/max:  {}/{}".format(runner_stats["cpu"]["min"],runner_stats["cpu"]["max"]))
    print("    mem min/max:  {}/{}".format(runner_stats["mem"]["min"],runner_stats["mem"]["max"]))
    print("    disk min/max: {}/{}".format(runner_stats["disk"]["min"],runner_stats["disk"]["max"]))

    if concord_stats:
        print("  Concord stats:")
        for ip in concord_stats:
            print("    {}".format(ip))
            print("      cpu min/max:  {}/{}".format(concord_stats[ip]["cpu"]["min"],concord_stats[ip]["cpu"]["max"]))
            print("      mem min/max:  {}/{}".format(concord_stats[ip]["mem"]["min"],concord_stats[ip]["mem"]["max"]))
            print("      disk min/max: {}/{}".format(concord_stats[ip]["disk"]["min"],concord_stats[ip]["disk"]["max"]))

    print("  All stats:")
    print("    cpu min/max:  {}/{}".format(all_stats["all"]["cpu"]["min"],all_stats["all"]["cpu"]["max"]))
    print("    mem min/max:  {}/{}".format(all_stats["all"]["mem"]["min"],all_stats["all"]["mem"]["max"]))
    print("    disk min/max: {}/{}".format(all_stats["all"]["disk"]["min"],all_stats["all"]["disk"]["max"]))


main()
