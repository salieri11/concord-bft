#!/bin/bash
#
# Extract and transform replica timing logs for use with d3 viewer.
#
# Usage: prepare_log.sh log_file [additional log files ...]
#
# Output is one log_file.json for each input, plus a filenames.js that
# lists those outputs.

if [ "$#" -lt 1 ]; then
    echo Usage: $0 log_file [additional log files ...]
fi

echo -n "var filenames = [" > filenames.js
FIRST_FILENAME=true

while [ "$1" ]; do
    if [ -z "$FIRST_FILENAME" ]; then
        echo -n "," >> filenames.js;
    else
        unset FIRST_FILENAME
    fi
    echo -n "\"$1.json\"" >> filenames.js

    echo "[" > "$1.json"
    # grep: find all timing logs (recognized by the "Gauges" fiels)
    # sed expression 1: extract time and data from log, add time to data
    #                   2019-08-30T14:23:05.123 some other words {"Name": ... other data} [/some/path]
    #                                  \                          /
    #                   {"Time":"2019-08-30T14:23:05.123","Name": ... other data},
    # sed expression 2: JSON doesn't allow trailing commas
    grep "Gauges" "$1" | sed -e "s/.*\([0-9]\{4\}-[^ ]*\).* {\(.*}\) .*/{\"Time\":\"\\1\",\\2,/" -e "$ s/,$//" >> "$1.json"
    echo "]" >> "$1.json"
    shift
done

echo "];" >> filenames.js
