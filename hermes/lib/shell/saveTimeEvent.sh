#!bin/bash

#########################################################################
# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
set +x

# Define EVENT_RECORDER and EVENT_FILE in the parent because the paths
# will be relative to that file.
EVENTS_FILE="PATH_TO_JSON_OUTPUT_FILE"
EVENTS_RECORDER="PATH_TO_event_recorder.py"

saveTimeEvent() {
    local STAGE="${1}"
    local EVENT="${2}"
    local CMD="python3 \"${EVENTS_RECORDER}\" record_event \"${STAGE}\" \"${EVENT}\" \"${EVENTS_FILE}\""
    eval "${CMD}"
}

