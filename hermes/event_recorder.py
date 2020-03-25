import datetime
import inspect
import json
import os
import shutil
import sys
import time
from collections import OrderedDict
from util import wavefront, racetrack

USAGE = "Usage:\n" \
        "python3 event_recorder.py record_event <stage_name> <event_name> <json_file>\n" \
        "python3 event_recorder.py summarize <json_file>"
ELAPSED_KEY = "elapsed"
EVENTS_KEY = "events"
TOTAL_TIME_KEY = "total"
TIME_FORMAT = "%Y-%m-%d_%H-%M-%S"
START_EVENT = "Start"
END_EVENT = "End"

def _waitForLockFile(lock_file_name):
    max_attempts = 10
    attempts = 0
    sleep_time = 1
    created = False

    while not created and attempts < max_attempts:
        attempts += 1

        try:
            with open(lock_file_name, "x") as f:
                created = True
        except Exception as e:
            print("Waiting to create lock file {}".format(lock_file_name))
            time.sleep(sleep_time)

    if not created:
        raise Exception("Could not obtain lock. " \
                        "Waited for file {}.".format(lock_file_name))


def _get_existing_events(events_file):
    '''
    Get any existing events from the stage file.
    '''
    events = {}

    try:
        if os.path.isfile(events_file):
            with open(events_file, "r") as f:
                events = json.loads(f.read(), object_pairs_hook=OrderedDict)
    except json.decoder.JSONDecodeError as e:
        print("File {} was not vaild json, overwriting it.".format(events_file))

    return events


def _calculate_elapsed(events):
    '''
    Calculate the duration of each stage and add it to the stage.
    Calculate the total duration as well.
    '''
    total_oldest = None
    total_newest = None

    for stage in events:
        if EVENTS_KEY in events[stage]:
            stage_oldest = None
            stage_newest = None

            for event in events[stage][EVENTS_KEY]:
                time_str = events[stage][EVENTS_KEY][event]
                event_time = datetime.datetime.strptime(time_str, TIME_FORMAT)

                if stage_newest == None or event_time > stage_newest:
                    stage_newest = event_time

                if total_newest == None or stage_newest > total_newest:
                    total_newest = stage_newest

                if stage_oldest == None or event_time < stage_oldest:
                    stage_oldest = event_time

                if total_oldest == None or stage_oldest < total_oldest:
                    total_oldest = stage_oldest

            stage_events = events[stage][EVENTS_KEY]
            if stage_oldest != stage_newest or ( # has time difference or
              START_EVENT in stage_events and END_EVENT in stage_events # < 1s event (started/ended right away)
            ): 
                diff = stage_newest - stage_oldest
                firstTimeCatchingDifference = True if not ELAPSED_KEY in events[stage] else False
                events[stage][ELAPSED_KEY] = str(diff)
                # if stage or test suite has ended, publish data on Wavefront and Racetrack
                if event == END_EVENT and firstTimeCatchingDifference:
                    startMili = int(stage_oldest.timestamp() * 1000)
                    durationMili = diff.seconds * 1000
                    wavefront.queueSpan(name=stage, start=startMili, duration=durationMili) # stage or test suite trace view (span)
                    wavefront.queueMetric(name=wavefront.WF_METRIC_STAGE_DURATION, value=diff.seconds, tags={
                        wavefront.WF_TAGNAME_STAGE: stage
                    }) # stage or test suite duration (line graph)


    if total_oldest and total_newest:
        # del() because we are using an OrderedDict and it's nice
        # to have the total time at the end.
        if TOTAL_TIME_KEY in events:
            del(events[TOTAL_TIME_KEY])

        events[TOTAL_TIME_KEY] = str(total_newest - total_oldest)


def record_event(stage_name=None, event_name=None, events_file=None):
    '''
    Adds the stage/event to the JSON in events_file, creating the file
    if necessary.
    '''
    events = {}
    lock_file = events_file + ".lck"
    created_lock = False

    if not stage_name or \
       not event_name or \
       not events_file:
        raise Exception(USAGE)

    try:
        now = datetime.datetime.now(tz=datetime.timezone.utc)
        now_str = now.strftime(TIME_FORMAT)
        _waitForLockFile(lock_file)
        created_lock = True
        events = _get_existing_events(events_file)

        if stage_name in events and \
           EVENTS_KEY in events[stage_name] and \
           event_name in events[stage_name][EVENTS_KEY]:
            print("WARNING: Already have value '{}' for stage '{}', event '{}'. " \
                  "New value '{}' will be ignored.".format(events[stage_name][EVENTS_KEY][event_name],
                                                           stage_name, event_name, now_str))
            return

        if not stage_name in events:
            events[stage_name] = {}

        if not EVENTS_KEY in events[stage_name]:
            events[stage_name][EVENTS_KEY] = {}

        events[stage_name][EVENTS_KEY][event_name] = now_str
        _calculate_elapsed(events)

        with open(events_file+"_tmp", "w") as f:
            f.write(json.dumps(events, indent=2))

        shutil.move(events_file+"_tmp", events_file)
    finally:
        if os.path.isfile(lock_file) and created_lock:
            os.remove(lock_file)


def summarize(events_file=None):
    '''
    Just print it nicely.
    '''
    if not events_file or \
       not os.path.isfile(events_file):
        raise Exception(USAGE)

    events = _get_existing_events(events_file)

    for stage_name in events:
        if stage_name != TOTAL_TIME_KEY:
            print(stage_name)
            events_found = False

            if EVENTS_KEY in events[stage_name] and \
               events[stage_name][EVENTS_KEY]:
                for event in sorted(events[stage_name][EVENTS_KEY], key=lambda e: events[stage_name][EVENTS_KEY][e]):
                    events_found = True
                    print("  {}: {}".format(event, events[stage_name][EVENTS_KEY][event]))

                if ELAPSED_KEY in events[stage_name]:
                    print("  Stage elapsed time: {}".format(events[stage_name][ELAPSED_KEY]))

            if not events_found:
                print("  No events")

    if TOTAL_TIME_KEY in events:
        print("Total elapsed time: {}".format(events[TOTAL_TIME_KEY]))

def _get_parameters(function_obj):
    '''
    Figure out the named parameters for the function we
    are calling and return an appropriate dict to call it.
    '''
    arg_names = inspect.getfullargspec(function_obj).args
    params = {}
    argv_index = 2

    for arg_name in arg_names:
        params[arg_name] = sys.argv[argv_index]
        argv_index += 1

    return params


if __name__ == "__main__":
    '''
    The first parameter passed in is the function to call.
    Other parameters passed in are passed to that function.
    '''
    if len(sys.argv) < 2:
        print(USAGE)
        exit(0)

    this_module = sys.modules[__name__]
    fnName = sys.argv[1]
    function_obj = getattr(this_module, fnName)
    function_params = _get_parameters(function_obj)
    function_obj(**function_params)
    
    # publish all metrics/spans in the queue
    wavefront.publish()
