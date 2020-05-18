# Copyright 2020 VMware, all rights reserved

import histogram
from threading import Thread
import time
import os
import io
import tee_pb2
import tee_pb2_grpc
import concord_pb2
import performance_pb2_grpc
import performance_pb2
import grpc
import threading
import math
import subprocess
import re
import sys
from datetime import datetime, timedelta
import sys

sys.path.insert(1,"../../../hermes/util/thin_replica")
import trutil
import thin_replica_pb2
import thin_replica_pb2_grpc

#  general params
servers=[("127.0.0.1","50051"),("127.0.0.1","50052"),("127.0.0.1", "50053"),("127.0.0.1","50054")]
#servers=[("127.0.0.1","50051"),]
clients_per_server = 4
num_of_requests = 1000

num_of_requests_per_uow =  int(num_of_requests / clients_per_server / len(servers))
warm_up_percentage = 10 # only after this % of requests script will start counting latencies and throughput
verbose = True

# for batch test
batch_test_block_count = num_of_requests
batch_test_kv_count = 300
batch_test_key_size = 12
batch_test_value_size = 2100
batch_test_payload_size = 730000

# for skvbc write test
request_size = 1000 # bytes

# for skvbc write test and TR test
block_key_size = 20
block_value_size = 900

# for TR test
trc_image_name = "trc-test-app:latest" # if used by CI, this is the defaut TRC test app image name
trc_container_name = None # if running container manually, one can asdd -name "trc-test-app" and then to use it here
thin_replica_client_id = "client_id_1"
num_of_updates = 64
kill_trc_container = False
# the total payload size (approx) will be num_of_updates*(block_key_size + block_value_size)
# 

class UnitOfWork:
    def __init__(self, id, request_response_list, response_aggregator_list=None):
        self.id_ = id
        self.requests_response_tuples_ = request_response_list
        self.histogram_ = histogram.Histogram()
        self.start_time_ = None
        self.end_time_ = None
        self.response_aggregator_list_ = response_aggregator_list

    def start(self):
        self.histogram_.clear()
        self.start_time_ = int(round(time.time() * 1000))
        num_of_requests = len(self.requests_response_tuples_)
        print(f"UOW {self.id_} started with {num_of_requests} iterations")
        self.count_ = 0
        warm_up_count = max(int(num_of_requests / 100 * warm_up_percentage), 0)
        for r in self.requests_response_tuples_:
            start_milli = int(round(time.time() * 1000))
            response = r[0](r[1])
            end_milli = int(round(time.time() * 1000))
            if warm_up_count > 0:
                warm_up_count -= 1
                continue
            elif warm_up_count == 0:
                print(f"UOW UOW {self.id_} done with warmup, starting measurement")
                warm_up_count -= 1
            if self.response_aggregator_list_ is not None:
                self.response_aggregator_list_.append((response, start_milli, end_milli))
            delta = end_milli - start_milli
            self.histogram_.add(delta)
            self.count_ += 1
            if verbose and self.count_ % max(int(num_of_requests * 0.1), 1) == 0:
                print(f"UOW {self.id_} progress: {self.count_} out of {num_of_requests} ({int((self.count_ / num_of_requests)*100)}%) done so far")
        self.end_time_ = int(round(time.time() * 1000))
        print(f"UOW {self.id_} done after {self.count_} iterations *************")

    def collect(self):
        return self.start_time_, self.end_time_, self.count_, self.histogram_, self.response_aggregator_list_


def do_job(uow):
    uow.start()


def print_histogram(total_count, start_time, end_time, hist, hist_name):
    buf = io.StringIO()
    buf.write("Performance summary for: ")
    buf.write(hist_name)
    buf.write("\n")
    buf.write(f"# of transactions: {total_count}")
    buf.write("\n")
    buf.write(f"Average throughput(tps): {round(total_count/(end_time - start_time)*1000)}")
    buf.write("\n")
    buf.write(hist.to_string())
    t = buf.getvalue()
    print(t)


def create_write_req_tr(server, key, value):
    ch = grpc.insecure_channel(f"{server[0]}:{server[1]}")
    stub = tee_pb2_grpc.TeeServiceStub(ch)
    kvdata = tee_pb2.KVData()

    for i in range(0, num_of_updates):
        tkv = tee_pb2.TridKV()
        tkv.trids.extend([thin_replica_client_id])
        key_list = [str(key), str(i)]
        key_list.extend(["k"] * (block_key_size - len(str(key))))
        tkv.key="".join(key_list)
        value_list = [str(value), str(i)]
        value_list.extend(["v"] * (block_value_size - len(str(key)) - len(thin_replica_client_id)))
        tkv.value = "".join(value_list)
        kvdata.trid_kvs.extend([tkv])
    return stub.WriteBlock,kvdata


def create_write_req(key, value, server):
    key_max_length = 21
    value_max_length = 21
    base_size = 25
    read_version_length = 8
    num_of_reads_length = 8
    num_of_updates_length = 8
    req_type = 2
    byte_order = "little"

    num_of_updates = int(math.floor((request_size - base_size) / (key_max_length + value_max_length)))
    num_of_updates_bytes = num_of_updates.to_bytes(num_of_updates_length, byte_order)
    key_bytes = key.to_bytes(key_max_length, byte_order)
    value_bytes = value.to_bytes(value_max_length, byte_order)
    req_type_bytes = req_type.to_bytes(1, byte_order)

    read_version_bytes = (0).to_bytes(read_version_length, byte_order)
    num_of_reads_bytes = (0).to_bytes(num_of_reads_length, byte_order)

    req_content = bytearray()
    ch = grpc.insecure_channel(f"{server[0]}:{server[1]}")
    stub = tee_pb2_grpc.TeeServiceStub(ch)
    r = tee_pb2.RawSkvbcRequest()
    # type - 1 byte
    # read_version - 8 bytes
    # num_of_reads - 8 bytes
    # num_of_updates - 8 bytes
    # {num_of_updates} of keyval sequences (key and value are 21 bytes each)
    req_content.extend(req_type_bytes)
    req_content.extend(read_version_bytes)
    req_content.extend(num_of_reads_bytes)
    req_content.extend(num_of_updates_bytes)
    for u in range(0,num_of_updates):
        req_content.extend(key_bytes)
        req_content.extend(value_bytes)
    r.content = bytes(req_content)
    return stub.SkvbcWrite, r


def create_perf_req(server, id, blockId, payload):
    ch = grpc.insecure_channel(f"{server[0]}:{server[1]}")
    stub = performance_pb2_grpc.PerformanceServiceStub(ch)
    write_req = performance_pb2.PerfWriteRequest()
    write_req.from_init.init_id = id
    write_req.from_init.block_id = blockId
    write_req.payload = payload
    return stub.PerfWrite, write_req


def send_write_tr():
    global trc_container_name
    global kill_trc_container

    trc_container_name = subprocess.run(['docker container ls | grep "trc-test-app:latest" | awk \'{print $1}\''], stdout=subprocess.PIPE, shell=True).stdout.decode("utf-8")
    if len(trc_container_name) == 0:
        print("start TRC test app")
        docker_str = f"docker run -d --network docker_default -p 50060:50060 {trc_image_name}" \
            f" ./test-app/trc_test_app {thin_replica_client_id}" \
            f" 50060 4 1 concord1:50051 concord2:50051 concord3:50051 concord4:50051"
        trc_container_name = subprocess.check_output([docker_str], shell=True).decode()
        tr = trutil.ThinReplica("127.0.0.1", "50060")
        tr_stream = tr.subscribe_to_updates(block_id=1, key_prefix=b"")
        time.sleep(20) #wait for TRC to load properly

    threads = []
    uows = []
    id = 1
    key = 1
    value = 1
    for s in servers:
        for i in range(0,clients_per_server):
            req_resp_list = []
            responses = []
            for r in range(0, num_of_requests_per_uow):
                req_tuple = create_write_req_tr(s, key, value)
                req_resp_list.append(req_tuple)
                key += 1
                value += 1
            w = UnitOfWork(id, req_resp_list, responses)
            uows.append(w)
            t = threading.Thread(target=do_job, args=(w,))
            threads.append(t)
            id += 1

    for t in threads:
        t.start()
    for t in threads:
        t.join()

    # collect loader results
    loader_times = [] # blocknum, start_time, end_time
    start_times = []
    end_times = []
    loader_hist = histogram.Histogram()
    loader_hist.clear()
    for uow in uows:
        res = uow.collect()
        responses = res[4]
        for response in responses:
            reg_exp = re.compile("\D+(\d+)")
            match = reg_exp.search(str(response[0]))
            loader_times.append((int(match[1]), response[1], response[2]))
            loader_hist.add(response[2] - response[1])
            start_times.append(response[1])
            end_times.append(response[2])

    # collect trc logs from the container
    print("collecting logs from ")
    trc_times = []

    while True:
        result = subprocess.run([f'docker logs --tail {len(loader_times) - len(trc_times)} {trc_container_name}'], stdout=subprocess.PIPE, shell=True)
        reg_exp = re.compile("(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3})\D+(\d+)")
        for group in reg_exp.findall(result.stdout.decode("utf-8")):
            utc_dt = datetime.strptime(group[0], "%Y-%m-%dT%H:%M:%S.%f")
            timestamp = int((utc_dt - datetime(1970, 1, 1)) / timedelta(milliseconds=1))
            trc_times.append((int(group[1]), timestamp))
        if len(loader_times) == len(trc_times):
            break
    assert(len(loader_times) == len(trc_times))

    # build e2e histogram
    e2e_hist = histogram.Histogram()
    e2e_hist.clear()
    loader_times.sort(key=lambda x: (-x[1], x[0]))
    trc_times.sort(key=lambda x: (-x[1], x[0]))
    start = loader_times[0][1]
    end = loader_times[0][2]
    for i in range(0, len(trc_times)):
        if loader_times[i][1] < start:
            start = loader_times[i][1]
        if loader_times[i][1] > end:
            end = trc_times[i][1]
        e2e_hist.add(trc_times[i][1] - loader_times[i][1])

    print_histogram(len(loader_times), min(start_times), max(end_times), loader_hist, "without trc")
    print_histogram(len(trc_times), start, end, e2e_hist, "with trc")

    # kill trc test app container
    if kill_trc_container:
        subprocess.check_output([f'docker kill {trc_container_name}'], shell=True).decode()

def send_write():
    perf_hist = histogram.Histogram()
    perf_hist.clear()
    threads = []
    uows = []
    id = 1
    key = 1
    value = 1
    for s in servers:
        for i in range(0,clients_per_server):
            req_resp_list = []
            for r in range(0, num_of_requests_per_uow):
                req_tuple = create_write_req(key, value, s)
                req_resp_list.append(req_tuple)
                key += 1
                value += 1
            w = UnitOfWork(id, req_resp_list)
            uows.append(w)
            t = threading.Thread(target=do_job, args=(w,))
            threads.append(t)
            id += 1

    for t in threads:
        t.start()
    for t in threads:
        t.join()

    start_times = []
    end_times = []
    total_count = 0
    for uow in uows:
        res = uow.collect()
        start_times.append(res[0])
        end_times.append(res[1])
        total_count += res[2]
        perf_hist.merge(res[3])

    start = min(start_times)
    end = max(end_times)
    print_histogram(total_count, start, end, perf_hist, "skvbc")


def clean_perf():
    init_msg = None
    for server in servers:
        ch = grpc.insecure_channel(f"{server[0]}:{server[1]}")
        stub = performance_pb2_grpc.PerformanceServiceStub(ch)
        init_req = performance_pb2.PerfCleanRequest()
        init_resp = stub.PerfClear(init_req)
        if not init_msg:
            init_msg = init_resp.message
        assert(init_msg == init_resp.message)


def run_perf_batch():
    perf_hist = histogram.Histogram()
    perf_hist.clear()
    threads = []
    uows = []
    block_id = 0
    payload = ("a" * batch_test_payload_size).encode()

    print("init")
    init_id = None
    for server in servers:
        ch = grpc.insecure_channel(f"{server[0]}:{server[1]}")
        stub = performance_pb2_grpc.PerformanceServiceStub(ch)
        init_req = performance_pb2.PerfInitRequest()
        init_req.block_count = batch_test_block_count
        init_req.kv_count = batch_test_kv_count
        init_req.key_size = batch_test_key_size
        init_req.value_size = batch_test_value_size
        init_resp = stub.PerfInit(init_req)
        if not init_id:
            init_id = init_resp.id
        assert(init_id == init_resp.id)
    print("init done")

    id = 1
    for s in servers:
        for i in range(0,clients_per_server):
            req_resp_list = []
            for r in range(0, num_of_requests_per_uow):
                req_tuple = create_perf_req(s, init_id,  block_id, payload)
                req_resp_list.append(req_tuple)
                block_id += 1
            w = UnitOfWork(id, req_resp_list)
            uows.append(w)
            t = threading.Thread(target=do_job, args=(w,))
            threads.append(t)
            id += 1

    for t in threads:
        t.start()
    for t in threads:
        t.join()

    clean_perf()

    start_times = []
    end_times = []
    total_count = 0
    for uow in uows:
        res = uow.collect()
        start_times.append(res[0])
        end_times.append(res[1])
        total_count += res[2]
        perf_hist.merge(res[3])

    start = min(start_times)
    end = max(end_times)
    print_histogram(total_count, start, end, perf_hist, "perf: " + init_id)


if __name__ == "__main__":
    # send_write()
    #send_write_tr()
    # run_perf_batch()
    print("choose script to run")