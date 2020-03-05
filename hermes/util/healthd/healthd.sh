#!/bin/bash

# kill previous health daemon
pkill -f "python3 healthd.py"

# run in background
python3 healthd.py > /dev/null 2>&1 &