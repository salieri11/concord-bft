import copy
import datetime
import json
import os
import pprint
import psutil
import pytz
import socket
import sys
import tempfile
import threading
import traceback
import time

import fixtures.common_fixtures
import util.hermes_logging
import util.wavefront

log = util.hermes_logging.getMainLogger()

# Wavefront metric strings
TOTAL_MEM_METRIC = "vmware.blockchain.mem.total"
AVAILABLE_MEM_METRIC = "vmware.blockchain.mem.available"
SYSTEM_CPU_METRIC = "vmware.blockchain.cpu.usage.system"
USER_CPU_METRIC = "vmware.blockchain.cpu.usage.user"
USED_DISK_METRIC = "vmware.blockchain.disk.used"

# Keys used for the output files
KEY_CPU_DATA = "cpu_data"
KEY_CPU_COUNT = "cpu_count"
KEY_CPU_FREQ = "cpu_freq_mhz"
KEY_CPU_SYSTEM = "system"
KEY_CPU_USER = "user"
KEY_CPU_TOTAL = "total"
KEY_CPU_USAGE_EACH = "cpu_usage_each"
KEY_CPU_USAGE_AVG = "cpu_usage_avg"
KEY_MEM_DATA = "mem_data"
KEY_MEM_TOTAL = "total_gb"
KEY_MEM_AVAIL = "available_gb"
KEY_MEM_UNAVAIL = "unavailable_gb"
KEY_DISK_USAGE = "disk_usage_gb"
KEY_NETWORK = "network_data"


class StatsGatherer(threading.Thread):
    def __init__(self, replicas_config=None, interval=3, overwrite=True):
        '''
        interval: How long to wait between samplings
        overwrite: Whether to overwrite existing data
        '''
        self._interval = interval
        self._replicas_config = replicas_config
        self._terminate = False

        # These are just in case nobody calls set_results_file().
        self._results_dir = "/tmp"
        self._results_file = os.path.join(self._results_dir, "stats.json")

        if os.path.exists(self._results_file) and overwrite:
            os.remove(self._results_file)

        super().__init__(target=self._monitor_fn)

        # So that threads will terminate when ctrl-c kills the parent process.
        self.daemon = True


    def set_results_file(self, results_dir):
        '''
        Once a run has gotten further along, it has figured out its results directory.
        Put the stats file in that directory.
        '''
        self._results_dir = results_dir
        self._results_file = os.path.join(self._results_dir, "stats.json")
        self._deployment_file = os.path.join(self._results_dir,
                                             fixtures.common_fixtures.DEPLOYED_BLOCKCHAIN_FILE)
        log.info("Stats file: {}".format(self._results_file))


    def _bytes_to_gb(self, b):
        '''
        Bytes to GB conversion and rounding to 2 decimal places.
        '''
        one_gb = 1024 * 1024 * 1024
        return round(b/one_gb, 2)


    def _gather_local_stats(self):
        '''
        Gather local stats and return in the structure:
        {
          localhost:
          {
            2020-11-09T01:18:37.662885+00:00: {
              mem_data: {
                total_gb: 12,
                available_gb: 9,
                unavailable_gb: 3
              },
              cpu_data: {
                cpu_count: 4,
                cpu_freq_ghz: 1.9
                cpu_usage_each: [1.0, 1.0, 99.0, 0.0]
                cpu_usage_avg: 25
              },
              disk_usage_gb: {
                /data: 68,
                /other: 10
              },
              network_data: {
                ips: ....
              }
            },
            ...
          }
        Remote stats are added at the end, retrieved from Wavefront.
        '''

        # Gather some memory stats.
        vmem = psutil.virtual_memory()
        total_mem = getattr(vmem, "total")
        available_mem = getattr(vmem, "available")
        unavailable_mem = total_mem - available_mem

        # Gather some disk stats.
        disk_usage = {}
        partitions = psutil.disk_partitions()
        for partition in partitions:
            path = getattr(partition, "mountpoint")
            usage = getattr(psutil.disk_usage(path), "used")
            disk_usage[path] = self._bytes_to_gb(usage)

        # Gather some network stats.
        network_stats = {}
        network = psutil.net_if_addrs()
        for interface in network:
            for snicaddr in network[interface]:
                if getattr(snicaddr, "family") == socket.AF_INET:
                    network_stats[interface] = getattr(snicaddr, "address")

        stats = {
            KEY_CPU_DATA: {
                KEY_CPU_COUNT: psutil.cpu_count(logical=False),
                KEY_CPU_FREQ: getattr(psutil.cpu_freq(percpu=False), "current"),
                KEY_CPU_USAGE_EACH: psutil.cpu_percent(interval=1, percpu=True),
                KEY_CPU_USAGE_AVG: psutil.cpu_percent(interval=1, percpu=False)
            },
            KEY_MEM_DATA: {
                KEY_MEM_TOTAL: self._bytes_to_gb(total_mem),
                KEY_MEM_AVAIL: self._bytes_to_gb(available_mem),
                KEY_MEM_UNAVAIL: self._bytes_to_gb(total_mem - available_mem)
            },
            KEY_DISK_USAGE: disk_usage,
            KEY_NETWORK: network_stats
        }

        ret = {datetime.datetime.now(datetime.timezone.utc).isoformat(): stats}
        # Keep this line for debugging, but don't print it for all test runs all the time.
        # log.debug("_gather_local_stats returning {}".format(pprint.pformat(ret, indent=4)))
        return ret


    def _add_normalized_local_stats(self, add_to_me):
        '''
        Normalize the local stats and add them with the passed in dict.
        '''
        add_to_me["localhost"] = {}

        with open(self._results_file, "r") as f:
            local_stats = json.load(f)

        for time_key in local_stats:
            new_key = datetime.datetime.fromisoformat(time_key).timestamp()
            add_to_me["localhost"][new_key] = copy.deepcopy(local_stats[time_key])
        
    
    def _write_file(self, stats, replace=False):
        '''
        stats: Stats to write
        replace: Whether to replace the contents of the file with stats.
          False will merge results.
          True will create a ".old" file, overwriting any previous .old file.
        For clean writes, write the file to a temp location, then move it as an
        atomic operation.
        '''
        old_stats = {}

        if os.path.exists(self._results_file):
            with open(self._results_file, "r") as f:
                old_stats = json.load(f)
            
            if replace:
                # Never read; this is for human troubleshooting.
                with open(self._results_file + ".old", "w+") as f_old:
                    f_old.write(json.dumps(old_stats, sort_keys=True, indent=4))

                old_stats = {}
                
        new_stats = {**stats, **old_stats}

        with open(tempfile.NamedTemporaryFile(delete=False).name, "w") as f:
            f.write(json.dumps(new_stats, sort_keys=True, indent=4))

        os.replace(f.name, self._results_file)


    def _monitor_fn(self):
        '''
        This is the function launched by the thread.  It performs a loop
        of gathering stats and writing them to disk until asked to termintate.
        '''
        while not self._terminate:
            stats = self._gather_local_stats()
            self._write_file(stats)
            time.sleep(self._interval)


    def _get_time_bounds(self):
        '''
        We have stored keys in ISO UTC format.
        Return the first and last entries which we created, as epoch timestamps.
        This covers the period from Pytest sessionstart through finish.
        '''
        stats = None

        with open(self._results_file, "r") as f:
            stats = json.load(f)

        times = sorted(stats)
        start_time = times[0]
        end_time = times[-1]

        start_time = datetime.datetime.fromisoformat(start_time).timestamp()
        end_time = datetime.datetime.fromisoformat(end_time).timestamp()

        log.debug("start_time timestamp: {}".format(start_time))
        log.debug("end_time timestamp: {}".format(end_time))

        return start_time, end_time


    def _get_populated_series(self, obj):
        '''
        Give a response from Wavefront, get the first populated data series.
        '''
        if obj and obj["timeseries"]:
            for series in obj["timeseries"]:
                if series["data"]:
                    return series


    def _merge(self, source, destination):
        '''
        Deep merge dicts.
        '''
        for key, value in source.items():
            if isinstance(value, dict):
                # get node or create one
                node = destination.setdefault(key, {})
                self._merge(value, node)
            else:
                destination[key] = value

        return destination


    def _add_remote_disk_stats(self, ip, start_time, end_time, dest):
        '''
        ip: IP address we will search Wavefront for
        start/end times: Epoch range (seconds)
        dest: Dict to which values will be added
        Returns nothing.  Modifies the passed in dict.
        '''
        total = None
        available = None
        new_stats = { ip: {} }

        # Format: "data":[[1604559600,1.6819478528E10],[1604559660,1.6819478528E10], ...]
        result = json.loads(util.wavefront.call_wavefront_chart_api(
            "ts({},{}={})".format(USED_DISK_METRIC, "vm_ip", ip),
            start_time, end_time))
        # Keep for debugging, use only when necessary.
        # log.debug("disk usage metric result: {}".format(pprint.pformat(result, indent=4)))

        for series in result["timeseries"]:
            if series["data"] and series["tags"]["path"]:
                path = series["tags"]["path"]

                for datum in series["data"]:
                    timestamp = datum[0]
                    disk_usage = self._bytes_to_gb(float(datum[1]))
                    self._merge({
                        ip: {
                            timestamp: {
                                KEY_DISK_USAGE: {
                                    path: disk_usage
                                }
                            }
                        }
                    }, new_stats)

        self._merge(new_stats, dest)


    def _add_remote_cpu_stats(self, ip, start_time, end_time, dest):
        '''
        ip: IP address we will search Wavefront for
        start/end times: Epoch range (seconds)
        dest: Dict to which values will be added
        Returns nothing.  Modifies the passed in dict.
        '''
        system = None
        user = None
        new_stats = { ip: {} }

        result = json.loads(util.wavefront.call_wavefront_chart_api(
            "ts({},{}={})".format(SYSTEM_CPU_METRIC, "vm_ip", ip),
            start_time, end_time))
        # Keep for debugging, use only when necessary.
        # log.debug("system cpu metric result: {}".format(pprint.pformat(result, indent=4)))

        for series in result["timeseries"]:
            if series["data"] and series["tags"]["cpu"] and series["tags"]["cpu"] == "cpu-total":
                for datum in series["data"]:
                    timestamp = datum[0]
                    system = round(datum[1], 2)
                    new_stats[ip][timestamp] = {
                        KEY_CPU_DATA: {
                            KEY_CPU_SYSTEM: system
                        }
                    }

        result = json.loads(util.wavefront.call_wavefront_chart_api(
            "ts({},{}={})".format(USER_CPU_METRIC, "vm_ip", ip),
            start_time, end_time))
        # Keep for debugging, use only when necessary.
        # log.debug("user cpu metric result: {}".format(pprint.pformat(result, indent=4)))
        
        for series in result["timeseries"]:
            if series["data"] and series["tags"]["cpu"] and series["tags"]["cpu"] == "cpu-total":
                for datum in series["data"]:
                    timestamp = datum[0]
                    user = round(datum[1], 2)
                    new_stats[ip][timestamp][KEY_CPU_DATA][KEY_CPU_USER] = user
                    new_stats[ip][timestamp][KEY_CPU_DATA][KEY_CPU_TOTAL] = round(system + user, 2)

        self._merge(new_stats, dest)


    def _add_remote_mem_stats(self, ip, start_time, end_time, dest):
        '''
        ip: IP address we will search Wavefront for
        start/end times: Epoch range (seconds)
        dest: Dict to which values will be added
        Returns nothing.  Modifies the passed in dict.
        '''
        total = None
        available = None
        new_stats = { ip: {} }

        # Format: "data":[[1604559600,1.6819478528E10],[1604559660,1.6819478528E10], ...]
        result = json.loads(util.wavefront.call_wavefront_chart_api(
            "ts({},{}={})".format(TOTAL_MEM_METRIC, "vm_ip", ip),
            start_time, end_time))
        # Keep for debugging, use only when necessary.
        # log.debug("total mem metric result: {}".format(pprint.pformat(result, indent=4)))
        series = self._get_populated_series(result)
        if series:
            for datum in series["data"]:
                timestamp = datum[0]
                total = self._bytes_to_gb(float(datum[1]))
                new_stats[ip][timestamp] = {
                    KEY_MEM_DATA: {
                        KEY_MEM_TOTAL: total
                    }
                }

        # Format: "data":[[1604559600,1.1476230826666666E10],[1604559660,1.1478081536E10],
        result = json.loads(util.wavefront.call_wavefront_chart_api(
            "ts({},{}={})".format(AVAILABLE_MEM_METRIC, "vm_ip", ip),
            start_time, end_time))
        # Keep for debugging, use only when necessary.
        # log.debug("available mem metric result: {}".format(pprint.pformat(result, indent=4)))
        series = self._get_populated_series(result)
        if series:
            for datum in series["data"]:
                timestamp = datum[0]
                available = self._bytes_to_gb(float(datum[1]))
                if not new_stats[ip][timestamp]:
                    new_stats[ip][timestamp] = {}

                new_stats[ip][timestamp][KEY_MEM_DATA][KEY_MEM_AVAIL] = available
                new_stats[ip][timestamp][KEY_MEM_DATA][KEY_MEM_UNAVAIL] = round(total - available, 2)

        self._merge(new_stats, dest)


    def _gather_remote_stats(self):
        '''
        To be called by the process which launched the StatsGatherer, when the history
        of the VMs needs to be gathered.
        If the user passed in a replicas_config.json file, read IP addresses from that.
        Otherwise, look for deployed_blockchains_short.json.  See infra.py, add_to_tracker_file
        to see how that path is determined.

        Adds the following to the json:
        {
          localhost: {...}, <---- Already created while a suite was running.
          10.1.2.3: {
            1604439458: {
              mem_data: {
              },
              cpu_data: {
              },
              disk_usage_gb: {
              }
            },
            ...
          },
          ...
        }
        '''
        ips = []
        new_stats = {}

        if self._replicas_config:
            parsed_config = util.helper.parseReplicasConfig(self._replicas_config)
            for k in parsed_config:
                ips.extend(parsed_config[k])
        elif os.path.exists(self._deployment_file):
            with open(self._deployment_file, "r") as f:
                config = json.load(f)

            for node in config["nodes_list"]:
                if node["public_ip"]:
                    ips.append(node["public_ip"])
                elif node["private_ip"]:
                    ips.append(node["private_ip"])
        else:
            log.info("No stats for deployed VMs were gathered")
            return new_stats

        start_time, end_time = self._get_time_bounds()

        for ip in ips:
            self._add_remote_mem_stats(ip, start_time, end_time, new_stats)
            self._add_remote_cpu_stats(ip, start_time, end_time, new_stats)
            self._add_remote_disk_stats(ip, start_time, end_time, new_stats)

        # Keep for debugging, use only when necessary.
        # log.debug("new_stats: {}".format(pprint.pformat(new_stats, indent=4)))
            
        return new_stats


    def request_stop(self):
        '''
        To be called by the process which launched the StatsGatherer.
        '''
        self._terminate = True

        try:
            all_stats = self._gather_remote_stats()
            self._add_normalized_local_stats(all_stats)
            self._write_file(all_stats, True)
        except Exception as e:
            log.info("Unable to gather remote stats.  Trapping this exception so it does not fail a run.")
            log.info(log.info(traceback.format_exc()))
