#!/bin/bash

# Ensure two arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <process_name> <nice_value>"
    echo "Example: $0 python 10"
    exit 1
fi

PROCESS_NAME="$1"
NICE_VALUE="$2"

# 1. Get the PGID of the process name
# (Takes the PGID of the oldest/parent matching process if multiple exist)
PGID=$(ps -C "$PROCESS_NAME" -o pgrp= | head -n 1 | tr -d ' ')

# Check if a valid PGID was found
if [ -z "$PGID" ]; then
    echo "Error: No running process found matching name '$PROCESS_NAME'."
    exit 1
fi

echo "Found Process Group ID (PGID) for '$PROCESS_NAME': $PGID"
echo "Applying nice value $NICE_VALUE to process group $PGID..."

# 2. Execute renice on the group
# (Using sudo automatically if the nice value is negative or if permissions fail)
if [ "$NICE_VALUE" -lt 0 ] 2>/dev/null; then
    sudo renice -n "$NICE_VALUE" -g "$PGID"
else
    renice -n "$NICE_VALUE" -g "$PGID" || sudo renice -n "$NICE_VALUE" -g "$PGID"
fi
