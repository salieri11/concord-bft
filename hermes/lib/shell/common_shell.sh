#!/bin/bash

# Define EVENT_RECORDER and EVENT_FILE in the parent.
EVENTS_FILE="PATH_TO_JSON_OUTPUT_FILE"
EVENTS_RECORDER="PATH_TO_event_recorder.py"

saveTimeEvent() {
    STAGE="${1}"
    EVENT="${2}"
    CMD="python3 \"${EVENTS_RECORDER}\" record_event \"${STAGE}\" \"${EVENT}\" \"${EVENTS_FILE}\""
    echo "CMD: ${CMD}"
    eval "${CMD}"
}
