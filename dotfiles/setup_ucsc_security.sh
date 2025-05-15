#!/bin/sh

if [ $# -eq 0 ]; then
    echo "Need to provide CRUZID"
    return;
fi

CRUZID="$1"

# Install the BigFix
sudo /bin/bash -u $CRUZID -c "$(curl -fsSL https://endpoint.ucsc.edu/open-downloads/bigfix/install_bigfix_linux.sh) -m isiponly"

# Install Endpoint Detection and Response --> Google Drive has it.

# Vulnerabilty Scans
# https://docs.google.com/document/d/1x-KWi5vFOqxNzXnkZJkUbrrE4xKdqhsuk16M-EVF_FE/edit?tab=t.0

# Duo Desktop
# https://duo.com/docs/duo-desktop#supported-operating-systems
